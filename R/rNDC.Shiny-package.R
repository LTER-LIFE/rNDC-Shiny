#' rNDC.GUI: graphical user interface to LTER-LIFE's NatureDataCube
#'
#' A Shiny app to find and get data from the NatureDataCube (and the related AgroDataCube and
#' GroenMonitor services), built on the functions of the \pkg{rNDC} package. Start it with [ndc_gui()].
#'
#' @import shiny
#' @importFrom leaflet leaflet leafletOutput renderLeaflet addTiles setView
#' @importFrom leaflet.extras addDrawToolbar editToolbarOptions
#' @importFrom shinyjs useShinyjs
#' @importFrom magrittr %>%
#' @importFrom lubridate as_datetime
#' @keywords internal
"_PACKAGE"

# Column names used with non-standard evaluation (dplyr, leaflet formulas) and the `.` pipe placeholder
utils::globalVariables(c(".", "layer_id", "month", "ndvi_mean", "ndvi_std"))
