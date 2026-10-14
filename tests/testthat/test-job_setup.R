# Retrieving in the background: when the mode is on, and with how many processes. (R/job_setup.R)

test_that("the background mode is off by default, and the options are only set when it is on", {
  expect_false(job_async_enabled())

  withr::local_options(rNDC.Shiny.async = NULL, rNDC.Shiny.workers = NULL, rNDC.Shiny.max_queue = NULL)
  withr::local_envvar(NDC_ASYNC = "false")
  expect_false(job_setup_async())
  expect_false(job_async_enabled())
  expect_null(getOption("rNDC.Shiny.workers"))

  withr::local_envvar(NDC_ASYNC = "true", NDC_WORKERS = "3", NDC_MAX_QUEUE = "7")
  expect_true(job_setup_async())
  expect_true(job_async_enabled())
  expect_equal(getOption("rNDC.Shiny.workers"), 3)
  expect_equal(getOption("rNDC.Shiny.max_queue"), 7)

  withr::local_envvar(NDC_WORKERS = "nonsense", NDC_MAX_QUEUE = "0")  # not usable: the defaults
  job_setup_async()
  expect_equal(getOption("rNDC.Shiny.workers"), job_default_workers())
  expect_equal(getOption("rNDC.Shiny.max_queue"), job_default_max_queue)
})

test_that("without NDC_ASYNC the mode follows whether the session is interactive", {
  withr::local_options(rNDC.Shiny.async = NULL)
  withr::local_envvar(NDC_ASYNC = "")
  expect_equal(job_setup_async(), !interactive())
})

test_that("a package that is only loaded needs pkgload for the workers", {
  expect_true(job_async_possible(dev = FALSE, pkgload = FALSE))
  expect_true(job_async_possible(dev = TRUE, pkgload = TRUE))
  expect_false(job_async_possible(dev = TRUE, pkgload = FALSE))
  expect_gte(job_default_workers(), 1)
  expect_lte(job_default_workers(), 4)
})
