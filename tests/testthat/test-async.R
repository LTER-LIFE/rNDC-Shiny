# Retrieving in background processes (R/async.R, R/jobs.R), with a queue and a cancel button.

raster <- function() terra::rast(nrows = 2, ncols = 2, vals = 1:4)

# ---- when and how ----

test_that("the background mode is off by default, and the options are only set when it is on", {
  expect_false(async_enabled())

  withr::local_options(rNDC.Shiny.async = NULL, rNDC.Shiny.workers = NULL, rNDC.Shiny.max_queue = NULL)
  withr::local_envvar(NDC_ASYNC = "false")
  expect_false(setup_async())
  expect_false(async_enabled())
  expect_null(getOption("rNDC.Shiny.workers"))

  withr::local_envvar(NDC_ASYNC = "true", NDC_WORKERS = "3", NDC_MAX_QUEUE = "7")
  expect_true(setup_async())
  expect_true(async_enabled())
  expect_equal(getOption("rNDC.Shiny.workers"), 3)
  expect_equal(getOption("rNDC.Shiny.max_queue"), 7)

  withr::local_envvar(NDC_WORKERS = "nonsense", NDC_MAX_QUEUE = "0")  # not usable: the defaults
  setup_async()
  expect_equal(getOption("rNDC.Shiny.workers"), default_workers())
  expect_equal(getOption("rNDC.Shiny.max_queue"), default_max_queue)
})

test_that("without NDC_ASYNC the mode follows whether the session is interactive", {
  withr::local_options(rNDC.Shiny.async = NULL)
  withr::local_envvar(NDC_ASYNC = "")
  expect_equal(setup_async(), !interactive())
})

test_that("a package that is only loaded needs pkgload for the workers", {
  expect_true(async_possible(dev = FALSE, pkgload = FALSE))
  expect_true(async_possible(dev = TRUE, pkgload = TRUE))
  expect_false(async_possible(dev = TRUE, pkgload = FALSE))
  expect_gte(default_workers(), 1)
  expect_lte(default_workers(), 4)
})

test_that("the longer limit and the wording follow the mode", {
  expect_equal(max_request_seconds(), 300)
  expect_match(long_retrieval_warning(120), "busy until it is done")

  withr::local_options(rNDC.Shiny.async = TRUE)
  expect_equal(max_request_seconds(), 900)
  expect_match(long_retrieval_warning(120), "runs in the background")
  expect_match(long_retrieval_warning(120), "cancel")

  withr::local_options(rNDC.Shiny.max_request_seconds = 100)  # an explicit limit always wins
  expect_equal(max_request_seconds(), 100)
})

# ---- progress between processes ----

test_that("the progress is written to a file and read back, whole or not at all", {
  file <- withr::local_tempfile()
  expect_null(read_progress(file))

  write <- progress_writer(file)
  write(detail = "Weather (A)", value = 0.1)
  expect_equal(read_progress(file), list(value = 0.1, detail = "Weather (A)"))

  write(value = 0.5)  # a part keeps the other
  expect_equal(read_progress(file), list(value = 0.5, detail = "Weather (A)"))
  write(detail = "next")
  expect_equal(read_progress(file), list(value = 0.5, detail = "next"))
  expect_false(file.exists(paste0(file, ".tmp")))

  writeLines("not an rds file", file)
  expect_null(read_progress(file))
})

# ---- the queue ----

test_that("jobs run up to the number of workers, and the others wait in order", {
  fake <- fake_manager(workers = 2)
  m <- fake$manager
  a <- job_submit(m, "f", list())
  b <- job_submit(m, "f", list())
  c <- job_submit(m, "f", list())
  d <- job_submit(m, "f", list())

  expect_equal(fake$rec$started, c(a, b))
  expect_equal(job_status(m, a)$state, "running")
  expect_equal(job_status(m, c)$state, "queued")
  expect_equal(c(job_status(m, c)$position, job_status(m, d)$position), c(1, 2))
  expect_equal(job_status(m, d)$queued, 2)
  expect_equal(job_status(m, a)$position, 0)

  # when a job ends, the next one starts, in the order of submission
  finish_fake_job(fake, a, value = "A")
  job_tick(m)
  expect_equal(fake$rec$started, c(a, b, c))
  expect_equal(job_status(m, d)$position, 1)
  expect_equal(job_status(m, a)$state, "done")  # finished jobs wait for their result to be collected
  expect_equal(job_collect(m, a)$value, "A")
  expect_null(job_status(m, a))  # and are then forgotten
})

test_that("too many waiting jobs are refused, but a free worker always takes one", {
  fake <- fake_manager(workers = 1, max_queue = 1)
  m <- fake$manager
  expect_false(is.null(job_submit(m, "f", list())))  # runs
  expect_false(is.null(job_submit(m, "f", list())))  # waits
  expect_null(job_submit(m, "f", list()))            # too many

  none <- fake_manager(workers = 1, max_queue = 0)$manager
  expect_false(is.null(job_submit(none, "f", list())))  # no queue is needed when a worker is free
  expect_null(job_submit(none, "f", list()))
})

test_that("a waiting job can be cancelled and the others move up", {
  fake <- fake_manager(workers = 1)
  m <- fake$manager
  a <- job_submit(m, "f", list())
  b <- job_submit(m, "f", list())
  c <- job_submit(m, "f", list())
  dir <- m$jobs[[b]]$dir

  expect_true(job_cancel(m, b))
  expect_equal(job_status(m, b)$state, "cancelled")
  expect_false(dir.exists(dir))
  expect_equal(job_status(m, c)$position, 1)
  expect_length(fake$rec$killed, 0)  # nothing was running for it
  expect_false(job_cancel(m, b))     # nothing left to cancel
  expect_equal(job_collect(m, b)$state, "cancelled")
})

test_that("a running job is killed, what it made is removed, and the next job takes its place", {
  fake <- fake_manager(workers = 1)
  m <- fake$manager
  made <- withr::local_tempfile()
  writeLines("zip", made)
  a <- job_submit(m, "f", list(), files = made)
  b <- job_submit(m, "f", list())
  dir <- m$jobs[[a]]$dir

  expect_true(job_cancel(m, a))
  expect_equal(fake$rec$killed, a)
  expect_false(file.exists(made))
  expect_false(dir.exists(dir))
  expect_equal(fake$rec$started, c(a, b))
  expect_equal(job_status(m, b)$state, "running")

  # a job that has ended cannot be cancelled any more
  finish_fake_job(fake, b, value = 1)
  job_tick(m)
  expect_false(job_cancel(m, b))
  expect_equal(job_collect(m, b)$value, 1)
})

test_that("a job that fails is reported with the reason", {
  fake <- fake_manager(workers = 2)
  m <- fake$manager
  a <- job_submit(m, "f", list())
  b <- job_submit(m, "f", list())

  finish_fake_job(fake, a, error = "it broke")
  finish_fake_job(fake, b, crash = TRUE)  # the process is gone and left nothing
  job_tick(m)
  expect_equal(job_status(m, a)$state, "failed")
  expect_equal(job_status(m, a)$error, "it broke")
  expect_equal(job_status(m, b)$state, "failed")
  expect_match(job_status(m, b)$error, "stopped unexpectedly.*stderr of the process")
  out <- job_collect(m, b)
  expect_equal(out$state, "failed")
  expect_null(out$value)

  # a process that cannot be started is a failure too, and does not block the queue
  broken <- new_job_manager(1, start = function(job) stop("no process"))
  id <- job_submit(broken, "f", list())
  expect_equal(job_status(broken, id)$state, "failed")
  expect_match(job_status(broken, id)$error, "could not be started: no process")
})

test_that("the progress of a running job is visible, and shutting down stops everything", {
  fake <- fake_manager(workers = 1)
  m <- fake$manager
  a <- job_submit(m, "f", list())
  b <- job_submit(m, "f", list())
  expect_null(job_status(m, a)$progress)
  progress_writer(m$jobs[[a]]$progress_file)(detail = "Weather (A)", value = 0.2)
  expect_equal(job_status(m, a)$progress, list(value = 0.2, detail = "Weather (A)"))
  expect_gte(job_status(m, a)$running, 0)

  dirs <- vapply(m$jobs, function(job) job$dir, "")
  job_shutdown(m)
  expect_equal(fake$rec$killed, a)
  expect_false(any(dir.exists(dirs)))
  expect_length(m$jobs, 0)
})

test_that("the text for a waiting user gives the position", {
  expect_match(queue_text(list(position = 2, queued = 3)), "Position 2 of 3 in the queue")
})

# ---- real processes ----

real_manager <- function(workers = 1) new_job_manager(workers, start = start_job_process)

# Tick until the job leaves "queued"/"running"
wait_for_job <- function(m, id) {
  wait_for(function() {
    job_tick(m)
    !job_status(m, id)$state %in% c("queued", "running")
  })
}

test_that("a job runs in a new R process, reports its progress and returns its value", {
  m <- real_manager()
  on.exit(job_shutdown(m), add = TRUE)
  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))), crs = 4326))
  # a dataset without retrieval: runs offline
  ov <- tibble::tibble(dataset = "Vegetation structure", view = "Geodata", year = NA_integer_, polygon = "p",
                       wkt = sf::st_as_text(sf::st_geometry(poly)), polygon_sf = list(poly),
                       date_from = as.Date(NA), date_to = as.Date(NA))
  id <- job_submit(m, "retrieve_and_package",
                   list(ov = ov, zipfile = NULL, save_files = FALSE, workdir = NULL, ndc_token = "t",
                        adc_token = "t", return_data = TRUE, pack = TRUE))
  wait_for_job(m, id)
  expect_equal(job_status(m, id)$state, "done", info = job_status(m, id)$error)
  expect_equal(read_progress(m$jobs[[id]]$progress_file)$value, 1)
  out <- job_collect(m, id)
  expect_equal(out$value$messages, "Skipped: Vegetation structure is not wired to a retrieval endpoint yet.")
  expect_false(out$value$produced_any)
})

test_that("an error in a process is reported with its message", {
  m <- real_manager()
  on.exit(job_shutdown(m), add = TRUE)
  id <- job_submit(m, "stop", list("it went wrong"))
  wait_for_job(m, id)
  expect_equal(job_status(m, id)$state, "failed")
  expect_equal(job_status(m, id)$error, "it went wrong")
})

test_that("cancelling a running job ends its process and removes its files", {
  m <- real_manager()
  on.exit(job_shutdown(m), add = TRUE)
  id <- job_submit(m, "Sys.sleep", list(120))
  expect_equal(job_status(m, id)$state, "running")
  handle <- m$jobs[[id]]$handle
  dir <- m$jobs[[id]]$dir
  expect_true(handle$is_alive())

  expect_true(job_cancel(m, id))
  wait_for(function() !handle$is_alive(), timeout = 15)
  expect_false(dir.exists(dir))
})

test_that("a process that dies without a result is a failed job", {
  m <- real_manager()
  on.exit(job_shutdown(m), add = TRUE)
  id <- job_submit(m, "quit", list(save = "no", status = 3))
  wait_for_job(m, id)
  expect_equal(job_status(m, id)$state, "failed")
  expect_match(job_status(m, id)$error, "stopped unexpectedly")
})

test_that("a job runs in a fresh process with the installed package", {
  # What is sent to a process must not depend on the session that sends it. This runs when the package is
  # installed (R CMD check): a fresh R process loads it, as in the container.
  skip_if(is_dev_package(), "the package is not installed")

  out <- system2(file.path(R.home("bin"), "Rscript"), test_path("scripts", "job-in-fresh-process.R"),
                 stdout = TRUE, stderr = TRUE,
                 env = paste0("R_LIBS=", paste(.libPaths(), collapse = .Platform$path.sep)))
  expect_true(any(grepl("JOB OK", out, fixed = TRUE)), info = paste(out, collapse = "\n"))
})

# ---- terra objects between processes ----

test_that("rasters and vectors survive being sent to another process when they are packed", {
  r <- raster()
  v <- terra::vect(sf::st_sf(a = 1, geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326)))

  roundtrip <- function(x) {
    f <- withr::local_tempfile()
    saveRDS(x, f)
    readRDS(f)
  }
  packed <- roundtrip(pack_result(r))
  expect_s4_class(packed, "PackedSpatRaster")
  expect_equal(terra::values(unpack_result(packed))[, 1], 1:4)
  expect_equal(nrow(unpack_result(roundtrip(pack_result(v)))), 1)

  # what is not a terra object is left alone
  df <- data.frame(a = 1)
  expect_identical(pack_result(df), df)
  expect_identical(unpack_result(df), df)
})

# ---- the retrieval of a whole overview, without Shiny ----

test_that("retrieve_and_package returns the data, or not, and wraps rasters when asked", {
  local_mocked_bindings(get_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  ov <- overview_row("Land Use", year = 2024L)
  calls <- list()
  progress <- function(detail = NULL, value = NULL) calls[[length(calls) + 1]] <<- list(detail = detail, value = value)

  res <- retrieve_and_package(ov, save_files = FALSE, ndc_token = "t", adc_token = "t", progress = progress)
  expect_s4_class(res$datasets$`Land Use_1`, "SpatRaster")
  expect_true(res$produced_any)
  expect_equal(res$messages, "Retrieved: Land Use raster for year 2024")
  expect_equal(calls[[1]], list(detail = "Land Use (Own polygon)", value = 0))
  expect_equal(calls[[length(calls)]], list(detail = NULL, value = 1))

  packed <- retrieve_and_package(ov, save_files = FALSE, ndc_token = "t", adc_token = "t", pack = TRUE)
  expect_s4_class(packed$datasets$`Land Use_1`, "PackedSpatRaster")

  none <- retrieve_and_package(ov, save_files = FALSE, ndc_token = "t", adc_token = "t", return_data = FALSE)
  expect_length(none$datasets, 0)
  expect_true(none$produced_any)
})

test_that("retrieve_and_package zips, and leaves nothing behind", {
  local_mocked_bindings(get_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  before <- list.files(tempdir(), "^ndc_export_")
  zip <- withr::local_tempfile(fileext = ".zip")

  res <- retrieve_and_package(overview_row("Land Use", year = 2024L), zipfile = zip, ndc_token = "t", adc_token = "t")
  expect_true(file.exists(zip))
  expect_null(res$out_dir)
  expect_equal(res$zipfile, zip)
  expect_identical(list.files(tempdir(), "^ndc_export_"), before)
})

# ---- the retrieval of a whole overview, without Shiny ----

test_that("retrieve_and_package returns the data, or not, and wraps rasters when asked", {
  local_mocked_bindings(get_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  ov <- overview_row("Land Use", year = 2024L)
  calls <- list()
  progress <- function(detail = NULL, value = NULL) calls[[length(calls) + 1]] <<- list(detail = detail, value = value)

  res <- retrieve_and_package(ov, save_files = FALSE, ndc_token = "t", adc_token = "t", progress = progress)
  expect_s4_class(res$datasets$`Land Use_1`, "SpatRaster")
  expect_true(res$produced_any)
  expect_equal(res$messages, "Retrieved: Land Use raster for year 2024")
  expect_equal(calls[[1]], list(detail = "Land Use (Own polygon)", value = 0))
  expect_equal(calls[[length(calls)]], list(detail = NULL, value = 1))

  packed <- retrieve_and_package(ov, save_files = FALSE, ndc_token = "t", adc_token = "t", pack = TRUE)
  expect_s4_class(packed$datasets$`Land Use_1`, "PackedSpatRaster")

  none <- retrieve_and_package(ov, save_files = FALSE, ndc_token = "t", adc_token = "t", return_data = FALSE)
  expect_length(none$datasets, 0)
  expect_true(none$produced_any)
})

test_that("retrieve_and_package zips, and leaves nothing behind", {
  local_mocked_bindings(get_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  before <- list.files(tempdir(), "^ndc_export_")
  zip <- withr::local_tempfile(fileext = ".zip")

  res <- retrieve_and_package(overview_row("Land Use", year = 2024L), zipfile = zip, ndc_token = "t", adc_token = "t")
  expect_true(file.exists(zip))
  expect_null(res$out_dir)
  expect_equal(res$zipfile, zip)
  expect_identical(list.files(tempdir(), "^ndc_export_"), before)
})

# ---- the app, with a background process ----

land_use_overview <- function() overview_row("Land Use", year = 2024L)

# a job manager that runs each job the moment it starts (in this process, so that the mocks of the test apply)
inline_jobs <- function(...) fake_manager(run = TRUE, ...)

test_that("a download is built in the background", {
  fake <- inline_jobs()
  local_jobs(fake$manager)
  rec <- new.env()
  rec$calls <- 0
  local_mocked_bindings(
    get_landuse_raster = function(...) {
      rec$calls <- rec$calls + 1
      list(stack = raster())
    },
    .package = "rNDC"
  )

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    settle(session, function() !retrieving())
    expect_null(job_id())
    expect_length(fake$rec$started, 1)
    expect_true(file.exists(prepared_zip()))
    expect_setequal(utils::unzip(prepared_zip(), list = TRUE)$Name,
                    c("land_use_geodata_own_polygon.tif", "own_polygon.gpkg", "download_summary.csv"))
    expect_equal(rec$calls, 1)
    expect_equal(download_msgs(), "Retrieved: Land Use raster for year 2024")
    expect_length(fake$manager$jobs, 0)  # the job is forgotten
  })
})

test_that("while a retrieval runs the page offers to cancel it, and a second click starts nothing", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))

  with_server({
    overview(land_use_overview())
    expect_false(grepl("Cancel retrieval", as.character(output$download_ui$html)))
    session$setInputs(check_and_download = 1)
    expect_true(retrieving())
    expect_false(is.null(job_id()))
    expect_null(prepared_zip())
    expect_match(as.character(output$download_ui$html), "Cancel retrieval")

    session$setInputs(check_and_download = 2)
    expect_length(fake$rec$started, 1)
    expect_true(any(grepl("already running", rec$notes)))

    # it ends: the progress follows the process and the result is taken
    progress_writer(fake$manager$jobs[[job_id()]]$progress_file)(detail = "Land Use", value = 0.4)
    session$elapse(500)
    expect_true(retrieving())
    finish_fake_job(fake, job_id(), value = list(datasets = list(), messages = "m", produced_any = FALSE))
    settle(session, function() !retrieving())
    expect_equal(download_msgs(), c("No data is available within your selection. Please try a different area, time period, or dataset.", "m"))
  })
})

test_that("'Return data to R' gets the data from the background, rasters included", {
  local_jobs(inline_jobs()$manager)
  rec <- new.env()
  local_mocked_bindings(get_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  local_mocked_bindings(return_data_to_r = function(res) rec$res <- res)

  with_server({
    overview(land_use_overview())
    session$setInputs(return_to_r = 1)
    settle(session, function() !is.null(rec$res))
    expect_s4_class(rec$res$datasets$`Land Use_1`, "SpatRaster")
    expect_equal(terra::values(rec$res$datasets$`Land Use_1`)[, 1], 1:4)
    expect_true(rec$res$produced_any)
    expect_false(retrieving())
  })
})

test_that("a retrieval that fails in the background is reported and the session can retrieve again", {
  local_jobs(inline_jobs()$manager)
  local_mocked_bindings(retrieve_row = function(...) stop("worker boom", call. = FALSE))
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    settle(session, function() !retrieving())
    expect_null(prepared_zip())
    expect_null(job_id())
  })
  expect_true(any(grepl("The retrieval failed: worker boom", rec$notes)))
})

test_that("the user can cancel a retrieval that runs", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))
  zips <- function() list.files(tempdir(), "\\.zip$")
  before <- zips()

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    id <- job_id()
    expect_equal(job_status(fake$manager, id)$state, "running")
    expect_match(as.character(output$download_ui$html), "Cancel retrieval")

    session$setInputs(cancel_retrieval = 1)
    settle(session, function() !retrieving())
    expect_equal(fake$rec$killed, id)
    expect_null(job_id())
    expect_null(prepared_zip())
    expect_match(as.character(output$download_ui$html), "Download dataset")
    expect_false(grepl("Cancel retrieval", as.character(output$download_ui$html)))

    # and a new retrieval can be started
    session$setInputs(check_and_download = 2)
    expect_false(is.null(job_id()))
    expect_equal(length(fake$rec$started), 2)
  })
  expect_true(any(grepl("cancelled", rec$notes)))
  expect_identical(zips(), before)
})

test_that("a retrieval waits in the queue while the workers are busy, and cancelling it needs no kill", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)
  busy <- job_submit(fake$manager, "f", list())  # someone else's job occupies the only worker

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    id <- job_id()
    st <- job_status(fake$manager, id)
    expect_equal(st$state, "queued")
    expect_equal(c(st$position, st$queued), c(1, 1))
    expect_true(retrieving())

    # the other job ends: this one starts
    finish_fake_job(fake, busy, value = NULL)
    settle(session, function() identical(job_status(fake$manager, id)$state, "running"))
    expect_equal(fake$rec$started, c(busy, id))

    # one more user queues, the first one gives up before its turn
    session$setInputs(cancel_retrieval = 1)
    settle(session, function() !retrieving())
    expect_equal(fake$rec$killed, id)
  })
})

test_that("a waiting retrieval is cancelled without a process to kill", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)
  busy <- job_submit(fake$manager, "f", list())

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    expect_equal(job_status(fake$manager, job_id())$state, "queued")
    session$setInputs(cancel_retrieval = 1)
    settle(session, function() !retrieving())
    expect_length(fake$rec$killed, 0)
    expect_equal(fake$rec$started, busy)
  })
})

test_that("a full queue refuses the retrieval with a message", {
  fake <- fake_manager(workers = 1, max_queue = 0)
  local_jobs(fake$manager)
  job_submit(fake$manager, "f", list())
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    expect_false(retrieving())
    expect_null(job_id())
  })
  expect_true(any(grepl("server is busy", rec$notes)))
})

test_that("a retrieval that runs when the user leaves is cancelled, and nothing is left behind", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)
  zips <- function() list.files(tempdir(), "\\.zip$")
  before <- zips()

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    id <- job_id()
    session$close()
    expect_equal(fake$rec$killed, id)
  })
  expect_length(fake$manager$jobs, 0)
  expect_identical(zips(), before)
})

test_that("a download that finished when the user leaves does not leave a zip behind", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    id <- job_id()
    zip <- fake$manager$jobs[[id]]$args$zipfile
    writeLines("zip", zip)  # what the job made
    finish_fake_job(fake, id, value = list(datasets = list(), messages = character(0), produced_any = TRUE))
    session$close()  # before the page has looked at the job
    expect_false(file.exists(zip))
  })
  expect_length(fake$manager$jobs, 0)
  expect_length(fake$rec$killed, 0)  # nothing to stop
})

test_that("without the background mode a download is built at once, as before", {
  local_mocked_bindings(get_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  expect_false(async_enabled())

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    expect_false(retrieving())
    expect_null(job_id())
    expect_true(file.exists(prepared_zip()))  # no waiting
  })
})
