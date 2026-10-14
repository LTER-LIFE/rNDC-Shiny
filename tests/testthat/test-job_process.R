# The process of a job: the progress file, real background processes, cancelling and the fresh process. (R/job_process.R)

test_that("the progress is written to a file and read back, whole or not at all", {
  file <- withr::local_tempfile()
  expect_null(job_progress_read(file))

  write <- job_progress_writer(file)
  write(detail = "Weather (A)", value = 0.1)
  expect_equal(job_progress_read(file), list(value = 0.1, detail = "Weather (A)"))

  write(value = 0.5)  # a part keeps the other
  expect_equal(job_progress_read(file), list(value = 0.5, detail = "Weather (A)"))
  write(detail = "next")
  expect_equal(job_progress_read(file), list(value = 0.5, detail = "next"))
  expect_false(file.exists(paste0(file, ".tmp")))

  writeLines("not an rds file", file)
  expect_null(job_progress_read(file))
})

real_manager <- function(workers = 1) job_manager(workers, start = job_start_process)

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
                   list(ov = ov, zipfile = NULL, save_files = FALSE, workdir = NULL, return_data = TRUE, pack = TRUE),
                   env = c(NDC_TOKEN = "t", ADC_TOKEN = "t"))
  wait_for_job(m, id)
  expect_equal(job_status(m, id)$state, "done", info = job_status(m, id)$error)
  expect_equal(job_progress_read(m$jobs[[id]]$progress_file)$value, 1)
  out <- job_collect(m, id)
  expect_equal(out$value$messages, "Skipped: Vegetation structure is not wired to a retrieval endpoint yet.")
  expect_false(out$value$produced_any)
})

test_that("a process gets the environment variables of its job", {
  m <- real_manager()
  on.exit(job_shutdown(m), add = TRUE)
  # the job is the function Sys.getenv(): what the process sees of the environment. (NDC_TOKEN and ADC_TOKEN
  # themselves are not used here: a ~/.Renviron that defines them takes precedence in a new R process.)
  id <- job_submit(m, "Sys.getenv", list(x = c("NDC_TEST_SECRET", "ADC_TEST_SECRET")),
                   env = c(NDC_TEST_SECRET = "secret-ndc", ADC_TEST_SECRET = "secret-adc"))
  wait_for_job(m, id)
  expect_equal(job_status(m, id)$state, "done", info = job_status(m, id)$error)
  expect_equal(unname(job_collect(m, id)$value), c("secret-ndc", "secret-adc"))
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
  skip_if(job_is_dev_package(), "the package is not installed")

  out <- system2(file.path(R.home("bin"), "Rscript"), test_path("scripts", "job-in-fresh-process.R"),
                 stdout = TRUE, stderr = TRUE,
                 env = paste0("R_LIBS=", paste(.libPaths(), collapse = .Platform$path.sep)))
  expect_true(any(grepl("JOB OK", out, fixed = TRUE)), info = paste(out, collapse = "\n"))
})

test_that("the manager reads the small status file, and the result only when it is collected", {
  fake <- fake_manager(workers = 1)
  m <- fake$manager
  id <- job_submit(m, "f", list())
  finish_fake_job(fake, id, value = "big")

  # a result that cannot be read does not show in the status ...
  writeBin(as.raw(1:10), m$jobs[[id]]$result_file)
  job_tick(m)
  expect_equal(job_status(m, id)$state, "done")

  # ... but is reported when it is collected
  out <- job_collect(m, id)
  expect_equal(out$state, "failed")
  expect_match(out$error, "could not be read")

  # an error of the job is in the status
  id2 <- job_submit(m, "f", list())
  finish_fake_job(fake, id2, error = "boom")
  job_tick(m)
  expect_equal(job_status(m, id2)$state, "failed")
  expect_equal(job_status(m, id2)$error, "boom")
})
