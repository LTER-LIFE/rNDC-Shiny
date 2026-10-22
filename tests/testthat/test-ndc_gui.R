# The launchers: ndc_gui() opens the app, ndc_gui2r() opens it with the button that returns the data to R. (R/ndc_gui.R)

# runApp() and the pieces around it replaced, to see what the launchers do: `rec$option` is the option for the button while the
# app "runs", `rec$args` the arguments of runApp()
local_launcher <- function(result = NULL, on_run = NULL, env = parent.frame()) {
  rec <- new.env()
  rec$shutdowns <- 0
  testthat::local_mocked_bindings(
    ndc_app = function() "the app",
    ndc_jobs = function() "the jobs",
    job_shutdown = function(jobs) rec$shutdowns <- rec$shutdowns + 1,
    .env = env
  )
  testthat::local_mocked_bindings(
    runApp = function(appDir, ...) {
      rec$option <- getOption("rNDC.Shiny.return_to_r")
      rec$args <- c(list(appDir), list(...))
      if (!is.null(on_run)) on_run()          # (what the app would do while it runs)
      if (inherits(result, "error")) stop(result)
      result
    },
    .package = "shiny", .env = env
  )
  rec
}

test_that("ndc_gui2r() runs the app with the button that returns the data, and gives what the button returns", {
  rec <- local_launcher(result = list(datasets = list(Weather_1 = 1)))
  local_mocked_bindings(session_is_interactive = function() TRUE)
  withr::local_options(rNDC.Shiny.return_to_r = NULL)

  expect_message(res <- ndc_gui2r(port = 4321, launch.browser = FALSE), "click \"Return data to R \\(close app\\)\"")
  expect_equal(res, list(datasets = list(Weather_1 = 1)))
  expect_true(rec$option)                                             # the button was on while the app ran ...
  expect_null(getOption("rNDC.Shiny.return_to_r"))                    # ... and the option is put back
  expect_equal(rec$args[[1]], "the app")
  expect_equal(rec$args$port, 4321)
  expect_equal(rec$args$host, "127.0.0.1")
  expect_false(rec$args$launch.browser)
  expect_equal(rec$shutdowns, 1)                                      # (the background processes are stopped)

  # other arguments go to runApp(); closing the app in another way gives NULL
  rec <- local_launcher(result = NULL)
  expect_null(suppressMessages(ndc_gui2r(quiet = TRUE)))
  expect_true(rec$args$quiet)
})

test_that("ndc_gui2r() puts the option back when the app fails, and needs an interactive session", {
  rec <- local_launcher(result = simpleError("the app failed"))
  local_mocked_bindings(session_is_interactive = function() TRUE)
  withr::local_options(rNDC.Shiny.return_to_r = FALSE)
  expect_error(suppressMessages(ndc_gui2r()), "the app failed")
  expect_false(getOption("rNDC.Shiny.return_to_r"))                   # as it was
  expect_equal(rec$shutdowns, 1)

  local_mocked_bindings(session_is_interactive = function() FALSE)
  rec <- local_launcher(result = list())
  expect_error(ndc_gui2r(), "needs an interactive R session")
  expect_null(rec$args)                                                # (the app was not started)
})

test_that("ndc_gui() never offers the button, and returns nothing visible", {
  rec <- local_launcher(result = NULL)
  withr::local_options(rNDC.Shiny.return_to_r = TRUE)                  # (even if it was on)
  expect_invisible(res <- ndc_gui(launch.browser = FALSE))
  expect_null(res)
  expect_false(rec$option)
  expect_true(getOption("rNDC.Shiny.return_to_r"))                     # and put back
})

test_that("the button is offered only by ndc_gui2r(), in an interactive session, and when nothing is running", {
  expect_false(return_to_r_offered())                                  # the default (and the tests are not interactive)
  local_mocked_bindings(session_is_interactive = function() TRUE)
  expect_false(return_to_r_offered())                                  # interactive, but not started by ndc_gui2r()
  withr::local_options(rNDC.Shiny.return_to_r = TRUE)
  expect_true(return_to_r_offered())
  local_mocked_bindings(session_is_interactive = function() FALSE)
  expect_false(return_to_r_offered())                                  # a container, say

  local_mocked_bindings(session_is_interactive = function() TRUE)
  with_server({
    overview(three_rows())
    session$flushReact()
    html <- as.character(output$download_ui$html)
    expect_match(html, "return_to_r", fixed = TRUE)                    # the button is there ...
    expect_match(html, "Return data to R (close app)", fixed = TRUE)
    options(rNDC.Shiny.return_to_r = FALSE)
    overview(three_rows()[1:2, ])
    session$flushReact()
    expect_no_match(as.character(output$download_ui$html), "return_to_r")   # ... and not when the app was started by ndc_gui()
  })
})

test_that("the app that ndc_gui() starts has no button to return the data, the one that ndc_gui2r() starts has", {
  local_mocked_bindings(session_is_interactive = function() TRUE)       # (an interactive session in both cases)
  withr::local_options(rNDC.Shiny.return_to_r = NULL)
  shown <- new.env()
  # the download area of the app, as the real server draws it while the launcher runs the app
  probe <- function() {
    with_server({
      overview(three_rows())
      session$flushReact()
      shown$html <- as.character(output$download_ui$html)
    })
  }

  local_launcher(on_run = probe)
  ndc_gui(launch.browser = FALSE)
  expect_match(shown$html, "check_and_download", fixed = TRUE)           # (the download button is there)
  expect_no_match(shown$html, "return_to_r")
  expect_no_match(shown$html, "Return data to R")

  suppressMessages(ndc_gui2r(launch.browser = FALSE))
  expect_match(shown$html, "check_and_download", fixed = TRUE)
  expect_match(shown$html, "return_to_r", fixed = TRUE)
  expect_match(shown$html, "Return data to R (close app)", fixed = TRUE)

  # and ndc_gui() again after ndc_gui2r(): the option was put back
  ndc_gui(launch.browser = FALSE)
  expect_no_match(shown$html, "return_to_r")
})
