# Retrieving in a background process, so that a long retrieval does not hold the app up.

raster <- function() terra::rast(nrows = 2, ncols = 2, vals = 1:4)

# ---- when and how ----

test_that("the background mode is off by default, and the plan is only set when it is on", {
  expect_false(async_enabled())

  skip_if_not(future::supportsMulticore())
  old <- future::plan(future::sequential)
  withr::defer(future::plan(old))

  withr::local_envvar(NDC_ASYNC = "false")
  expect_false(setup_async())
  expect_false(async_enabled())
  expect_s3_class(future::plan(), "sequential")

  withr::local_options(rNDC.Shiny.async = NULL)
  withr::local_envvar(NDC_ASYNC = "true", NDC_WORKERS = "3")
  expect_true(setup_async())
  expect_true(async_enabled())
  expect_false(inherits(future::plan(), "sequential"))
  expect_equal(future::nbrOfWorkers(), 3)
})

test_that("without NDC_ASYNC the mode follows whether the session is interactive", {
  skip_if_not(future::supportsMulticore())
  old <- future::plan(future::sequential)
  withr::defer(future::plan(old))
  withr::local_options(rNDC.Shiny.async = NULL)
  withr::local_envvar(NDC_ASYNC = "", NDC_WORKERS = "1")
  expect_equal(setup_async(), !interactive())
})

test_that("a plan that was set already is kept", {
  skip_if_not(future::supportsMulticore())
  old <- future::plan(future::multicore, workers = 2)
  withr::defer(future::plan(old))
  withr::local_options(rNDC.Shiny.async = NULL)
  withr::local_envvar(NDC_ASYNC = "true", NDC_WORKERS = "5")
  setup_async()
  expect_equal(future::nbrOfWorkers(), 2)
})

test_that("the kind of process depends on whether the package is installed", {
  expect_identical(async_strategy(dev = FALSE, multicore = FALSE), future::multisession)
  expect_identical(async_strategy(dev = FALSE, multicore = TRUE), future::multisession)
  expect_identical(async_strategy(dev = TRUE, multicore = TRUE), future::multicore)
  expect_null(async_strategy(dev = TRUE, multicore = FALSE))
  expect_gte(default_workers(), 1)
  expect_lte(default_workers(), 4)
})

test_that("the longer limit and the wording follow the mode", {
  expect_equal(max_request_seconds(), 300)
  expect_match(long_retrieval_warning(120), "busy until it is done")

  withr::local_options(rNDC.Shiny.async = TRUE)
  expect_equal(max_request_seconds(), 900)
  expect_match(long_retrieval_warning(120), "runs in the background")

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

test_that("a worker's progress reaches the main process through the file", {
  skip_if_not(future::supportsMulticore())
  progress_file <- withr::local_tempfile()
  job <- function(write) { write(detail = "in the worker", value = 0.7); "done" }
  f <- future::future(job(progress_writer(progress_file)),
                      globals = list(job = job, progress_writer = progress_writer, progress_file = progress_file),
                      seed = NULL, lazy = FALSE)
  expect_equal(future::value(f), "done")
  expect_equal(read_progress(progress_file), list(value = 0.7, detail = "in the worker"))
})

test_that("the progress bar of the page follows the file, and cleans up", {
  session <- shiny::MockShinySession$new()
  file <- withr::local_tempfile()
  progress_writer(file)(detail = "Weather (A)", value = 0.3)

  shiny::withReactiveDomain(session, {
    p <- async_progress(session, file)
    session$flushReact()
    p$close()
  })
  expect_false(file.exists(file))
})

test_that("a job runs in a fresh background process, with the package installed there", {
  # New R processes load the installed package, so this runs when the package is installed (R CMD check),
  # not with load_all() (the tests above fork the process instead). Forked processes cannot show what goes
  # wrong when what is sent to a process depends on the session that sends it (see progress_writer()).
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

# ---- the app, with a background process ----

land_use_overview <- function() overview_row("Land Use", year = 2024L)

test_that("a download is built in the background while the app stays responsive", {
  local_async()
  rec <- new.env()
  rec$marker <- withr::local_tempfile()
  local_mocked_bindings(
    get_landuse_raster = function(...) {
      cat("x\n", file = rec$marker, append = TRUE)
      Sys.sleep(1.5)
      list(stack = raster())
    },
    .package = "rNDC"
  )

  with_server({
    overview(land_use_overview())
    started <- Sys.time()
    session$setInputs(check_and_download = 1)
    expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)  # the click came back at once
    expect_true(retrieving())
    expect_null(prepared_zip())

    # a second click while it runs does not start another retrieval
    session$setInputs(check_and_download = 2)

    wait_for(function() !retrieving())
    expect_gte(as.numeric(difftime(Sys.time(), started, units = "secs")), 1.5)
    expect_true(file.exists(prepared_zip()))
    expect_setequal(utils::unzip(prepared_zip(), list = TRUE)$Name,
                    c("land_use_geodata_own_polygon.tif", "own_polygon.gpkg", "download_summary.csv"))
    expect_equal(length(readLines(rec$marker)), 1)  # once
    expect_equal(download_msgs(), "Retrieved: Land Use raster for year 2024")
  })
})

test_that("'Return data to R' gets the data from the background, rasters included", {
  local_async()
  rec <- new.env()
  local_mocked_bindings(get_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  local_mocked_bindings(return_data_to_r = function(res) rec$res <- res)

  with_server({
    overview(land_use_overview())
    session$setInputs(return_to_r = 1)
    wait_for(function() !is.null(rec$res))
    expect_s4_class(rec$res$datasets$`Land Use_1`, "SpatRaster")
    expect_equal(terra::values(rec$res$datasets$`Land Use_1`)[, 1], 1:4)
    expect_true(rec$res$produced_any)
    expect_false(retrieving())
  })
})

test_that("a retrieval that fails in the background is reported and the session can retrieve again", {
  local_async()
  local_mocked_bindings(retrieve_row = function(...) stop("worker boom", call. = FALSE))

  messages <- testthat::capture_messages(
    with_server({
      overview(land_use_overview())
      session$setInputs(check_and_download = 1)
      expect_true(retrieving())
      wait_for(function() !retrieving())
      expect_null(prepared_zip())
      later::run_now(0.5)  # let a late error of a callback show up
    })
  )
  expect_false(any(grepl("Unhandled promise error|Unexpected error", messages)), info = paste(messages, collapse = "; "))
})

test_that("a download that finishes after the user has left does not leave a zip behind", {
  local_async()
  rec <- new.env()
  rec$done <- withr::local_tempfile()
  local_mocked_bindings(
    get_landuse_raster = function(...) {
      Sys.sleep(0.5)
      list(stack = raster())
    },
    .package = "rNDC"
  )
  zips <- function() list.files(tempdir(), "\\.zip$")
  before <- zips()

  messages <- testthat::capture_messages(
    with_server({
      overview(land_use_overview())
      session$setInputs(check_and_download = 1)
      expect_true(retrieving())
      session$close()
      wait_for(function() !retrieving())
      expect_null(prepared_zip())
      later::run_now(0.5)
    })
  )
  expect_false(any(grepl("Unhandled promise error|Unexpected error", messages)), info = paste(messages, collapse = "; "))
  expect_identical(zips(), before)
})

test_that("without the background mode a download is built at once, as before", {
  local_mocked_bindings(get_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  expect_false(async_enabled())

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    expect_false(retrieving())
    expect_true(file.exists(prepared_zip()))  # no waiting
  })
})
