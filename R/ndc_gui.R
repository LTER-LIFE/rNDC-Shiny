#' Launch the NatureDataCube GUI
#'
#' Run the Shiny app that gives a graphical user interface to the NatureDataCube. To continue working
#' with the retrieved data in R, assign the result (`data_ndc <- ndc_gui()`) and use the
#' "Return data to R (close app)" button of the app (only offered in interactive sessions).
#'
#' @param host character. IP address to listen on (use `"0.0.0.0"` to accept external connections, e.g.
#'   in a container).
#' @param port integer. Port to listen on. If `NULL` (default), a random port is chosen.
#' @param launch.browser logical or function. Open the app in a browser? Defaults to `TRUE` in interactive
#'   sessions.
#' @param ... further arguments passed to [shiny::runApp()].
#' @returns The list of data returned by the "Return data to R" button (with the elements `datasets`,
#'   `overview`, `messages` and `summary`), or `NULL` if the app is closed in any other way.
#' @seealso [ndc_app()] for the credentials the app needs.
#' @export

ndc_gui <- function(host = "127.0.0.1", port = NULL,
                    launch.browser = getOption("shiny.launch.browser", interactive()), ...) {
  # the app may start background processes for retrieving (see ndc_app()): leave the session as it was
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  shiny::runApp(ndc_app(), host = host, port = port, launch.browser = launch.browser, ...)
}

# Close the app and give `res` to the R session that started it ("Return data to R")
return_data_to_r <- function(res) shiny::stopApp(res)
