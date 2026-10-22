#' Launch the NatureDataCube GUI
#'
#' Run the Shiny app that gives a graphical user interface to the NatureDataCube. `ndc_gui()` opens the app to choose,
#' check and download datasets (a zip with the data). `ndc_gui2r()` opens the same app with one more button, "Return data
#' to R (close app)": it closes the app and gives the retrieved data to the R session, to continue working with it
#' (`data_ndc <- ndc_gui2r()`). That needs an interactive R session.
#'
#' @param host character. IP address to listen on (use `"0.0.0.0"` to accept external connections, e.g.
#'   in a container).
#' @param port integer. Port to listen on. If `NULL` (default), a random port is chosen.
#' @param launch.browser logical or function. Open the app in a browser? Defaults to `TRUE` in interactive
#'   sessions.
#' @param ... further arguments passed to [shiny::runApp()].
#' @returns `ndc_gui()`: `NULL`, invisibly, when the app is closed. `ndc_gui2r()`: the list of data returned by the
#'   "Return data to R" button (with the elements `datasets`, `overview`, `messages`, `summary` and `parameters`), or `NULL`
#'   if the app is closed in any other way.
#' @seealso [ndc_app()] for the credentials the app needs.
#' @examples
#' \dontrun{
#' ndc_gui()                      # choose and download datasets
#' data_ndc <- ndc_gui2r()        # choose datasets and get them in R: click "Return data to R (close app)"
#' names(data_ndc$datasets)
#' }
#' @export

ndc_gui <- function(host = "127.0.0.1", port = NULL,
                    launch.browser = getOption("shiny.launch.browser", interactive()), ...) {
  # (the button that returns the data to R is for ndc_gui2r())
  old <- options(rNDC.Shiny.return_to_r = FALSE)
  on.exit(options(old), add = TRUE)
  invisible(ndc_gui_run(host, port, launch.browser, ...))
}

#' @rdname ndc_gui
#' @export

ndc_gui2r <- function(host = "127.0.0.1", port = NULL,
                      launch.browser = getOption("shiny.launch.browser", interactive()), ...) {
  if (!session_is_interactive()) {
    stop("ndc_gui2r() needs an interactive R session: the data is returned to the R session that runs the app. ",
         "Use ndc_gui() to download the data as a zip.", call. = FALSE)
  }
  old <- options(rNDC.Shiny.return_to_r = TRUE)
  on.exit(options(old), add = TRUE)
  message("Choose the datasets, and click \"Return data to R (close app)\" to get them in R.")
  ndc_gui_run(host, port, launch.browser, ...)
}

# Run the app (the same for both launchers)
ndc_gui_run <- function(host, port, launch.browser, ...) {
  # the app may start background processes for retrieving (see ndc_app()): stop them when the app ends
  on.exit(job_shutdown(ndc_jobs()), add = TRUE)
  shiny::runApp(ndc_app(), host = host, port = port, launch.browser = launch.browser, ...)
}

# Close the app and give `res` to the R session that started it ("Return data to R")
return_data_to_r <- function(res) shiny::stopApp(res)

# Is this an interactive R session? (a function of its own, so that the tests can say)
session_is_interactive <- function() interactive()

# Does the app offer the button "Return data to R (close app)"? Only when it was started with ndc_gui2r(), in an interactive session
return_to_r_offered <- function() isTRUE(getOption("rNDC.Shiny.return_to_r")) && session_is_interactive()
