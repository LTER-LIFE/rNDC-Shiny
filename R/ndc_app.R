#' NatureDataCube Shiny app
#'
#' Create the Shiny app object of the graphical user interface to the NatureDataCube. Use [ndc_gui()]
#' to run it from R; `ndc_app()` itself is meant for deployments that need an app object (Shiny Server,
#' `shiny::runApp()`, tests).
#'
#' The app needs the environment variable `NDC_TOKEN` (NatureDataCube API token). `ADC_TOKEN`
#' (AgroDataCube API token) is optional: without it the datasets from AgroDataCube (Weather, Soil map,
#' AHN and Agricultural fields) are disabled. `SHINY_APP_BASE_URL` sets the proxy path when the app runs
#' behind a reverse proxy (e.g. `/naturedatacube`).
#'
#' @returns A `shiny.appobj`.
#' @seealso [ndc_gui()]
#' @export

ndc_app <- function() {
  ndc_setup()
  shiny::addResourcePath("ndc-www", system.file("app", "www", package = "rNDC.GUI"))
  shiny::shinyApp(app_ui(), app_server)
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
  invisible(TRUE)
}
