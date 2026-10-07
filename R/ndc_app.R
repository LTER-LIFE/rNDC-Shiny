#' NatureDataCube Shiny app
#'
#' Create the Shiny app object of the graphical user interface to the NatureDataCube. Use [ndc_gui()]
#' to run it from R; `ndc_app()` itself is meant for deployments that need an app object (Shiny Server,
#' `shiny::runApp()`, tests).
#'
#' The app needs the environment variable `NDC_TOKEN` (NatureDataCube API token). `ADC_TOKEN`
#' (AgroDataCube API token) is optional: without it the datasets from AgroDataCube (Weather, Soil map,
#' AHN and Agricultural fields) are disabled. `SHINY_APP_BASE_URL` sets the proxy path when the app runs
#' behind a reverse proxy (e.g. `/naturedatacube`). `NDC_MAX_UPLOAD_MB` sets the largest upload (default 100 MB;
#' Shiny's own default is 5 MB). `NDC_MAX_REQUEST_SECONDS` sets the longest retrieval that
#' is accepted (default 300 seconds): the overview is estimated when a dataset is added, and a retrieval keeps
#' the app busy until it is done.
#'
#' In a non-interactive session (a deployment, e.g. the Docker image) the retrievals run in background R processes,
#' so that a long one does not hold the other users up; `NDC_ASYNC` (`true` or `false`) chooses the mode, and
#' `NDC_WORKERS` sets the number of processes that run at the same time (default: up to 4). A retrieval that
#' finds all of them busy waits in a queue (the page shows its position; `NDC_MAX_QUEUE`, default 20, is the
#' number that may wait), and can be cancelled. In an interactive R session retrieval happens in the session
#' itself, unless `NDC_ASYNC=true`.
#'
#' @returns A `shiny.appobj`.
#' @seealso [ndc_gui()]
#' @export

ndc_app <- function() {
  ndc_setup()
  setup_async()
  shiny::addResourcePath("ndc-www", system.file("app", "www", package = "rNDC.Shiny"))
  shiny::shinyApp(app_ui(), app_server, onStart = function() {
    shiny::onStop(function() job_shutdown(ndc_jobs()))
  })
}

# Check the credentials and set the options that the app needs before it starts.
ndc_setup <- function() {
  if (!nzchar(Sys.getenv("NDC_TOKEN"))) {
    stop("NDC_TOKEN environment variable is not set. Add it to your .env file or set it with Sys.setenv().",
         call. = FALSE)
  }
  if (!nzchar(Sys.getenv("ADC_TOKEN"))) {
    warning("ADC_TOKEN is not set: Weather, Soil map, AHN and Agricultural fields are disabled.",
            call. = FALSE)
  }
  app_base_url <- Sys.getenv("SHINY_APP_BASE_URL")
  if (nzchar(app_base_url)) options(shiny.appBaseUrl = app_base_url)
  # uploads: Shiny's default of 5 MB is too small for many shapefiles and GeoPackages. A limit that was set
  # already (the option) is kept, unless NDC_MAX_UPLOAD_MB says otherwise.
  upload_mb <- suppressWarnings(as.numeric(Sys.getenv("NDC_MAX_UPLOAD_MB")))
  if (!is.na(upload_mb) && upload_mb > 0) {
    options(shiny.maxRequestSize = upload_mb * 1024^2)
  } else if (is.null(getOption("shiny.maxRequestSize"))) {
    options(shiny.maxRequestSize = default_upload_mb * 1024^2)
  }
  max_seconds <- suppressWarnings(as.numeric(Sys.getenv("NDC_MAX_REQUEST_SECONDS")))
  if (!is.na(max_seconds) && max_seconds > 0) options(rNDC.Shiny.max_request_seconds = max_seconds)
  invisible(TRUE)
}
