# Retrieval of the datasets in the overview, one function per dataset.
#
# `retrieve_dataset()` calls the retrieval function of the dataset of a row (`retrieve_<dataset>()`), which takes the
# row (a one-row tibble with the columns of the overview) and returns an "outcome", made with
# `retrieve_outcome_data()` or `retrieve_outcome_none()`. `retrieve_row()` (R/retrieve_row.R) writes the file and
# describes the outcome for the download summary and the messages panel.

# The dataset was retrieved. `name` is the prefix of its name in the returned list (the row number is
# added), `ext` the file extension, `ok_msg` / `fail_msg` the messages for a retrieved dataset / a file that
# could not be written, and `writer` the function that writes it (`function(path)`, TRUE if it worked).
retrieve_outcome_data <- function(data, name, ext, ok_msg, fail_msg, writer = NULL) {
  if (is.null(writer)) writer <- function(path) export_write_data(data, path)
  list(data = data, name = name, ext = ext, ok_msg = ok_msg, fail_msg = fail_msg, writer = writer)
}

# Nothing was retrieved: `message` for the messages panel, `file_type` and `status` for the download
# summary.
retrieve_outcome_none <- function(message, file_type, status = "failed") {
  list(data = NULL, message = message, file_type = file_type, status = status)
}

# The retrieval functions of rNDC can record their parameters (`save_params`, see rNDC::ndc_repeat()). A retrieval
# records them in files that start with `stem` (NULL: not at all): `retrieve_params_path(stem)` is the path for the file of the
# data, `retrieve_params_path(stem, "station")` for another call of the same dataset. Afterwards the files are moved to their
# place in the download, or read (see `retrieve_row()`).
retrieve_params_path <- function(stem, slot = "data") if (is.null(stem)) NULL else paste0(stem, "_", slot, ".json")

# File type of the vector datasets: a table for the Statistics view, a GeoPackage otherwise.
retrieve_vector_ext <- function(row) if (tolower(as.character(row$view)) == "statistics") "csv" else "gpkg"

# AgroDataCube answers with GeoJSON
retrieve_adc_outcome <- function(row, response, geometry = list(tolerance = 0)) {
  data <- geojsonsf::geojson_sf(jsonlite::toJSON(response, auto_unbox = TRUE))
  retrieve_outcome_data(data, row$dataset, retrieve_vector_ext(row),
               ok_msg = paste0("Retrieved: ", row$dataset, adc_simplified_note(geometry)),
               fail_msg = paste0("Failed: ", row$dataset, " - could not write output"))
}

retrieve_agricultural_fields <- function(row, adc_token, stem = NULL) {
  geometry <- adc_geometry(row)
  response <- rNDC::adc_get_all("Fields", c(geometry = geometry$wkt, epsg = "4326", year = row$year, output_epsg = "4326"),
                                token = adc_token, save_params = retrieve_params_path(stem))
  retrieve_adc_outcome(row, response, geometry)
}

retrieve_ahn <- function(row, adc_token, stem = NULL) {
  geometry <- adc_geometry(row)
  url <- rNDC::adc_url("AHN", params = c(geometry = geometry$wkt, epsg = "4326"))
  retrieve_adc_outcome(row, rNDC::adc_get(url = url, token = adc_token, save_params = retrieve_params_path(stem)), geometry)
}

retrieve_soil_map <- function(row, adc_token, stem = NULL) {
  geometry <- adc_geometry(row)
  response <- rNDC::adc_get_all("Soiltypes", c(geometry = geometry$wkt, epsg = "4326", output_epsg = "4326"),
                                token = adc_token, save_params = retrieve_params_path(stem))
  retrieve_adc_outcome(row, response, geometry)
}

retrieve_weather <- function(row, adc_token, stem = NULL) {
  ext <- retrieve_vector_ext(row)
  failed <- function(message) retrieve_outcome_none(paste0("Failed: Weather - ", message), ext)

  closest_id <- rNDC::adc_weather_station(row$wkt, token = adc_token,
                                          save_params = retrieve_params_path(stem, "station"))$closest_id
  # adc_weather_station() gives character(0) (not NULL) when no id is found
  if (length(closest_id) != 1 || is.na(closest_id) || !nzchar(closest_id)) {
    return(failed("no nearby station found"))
  }
  if (is.na(row$date_from) || is.na(row$date_to)) return(failed("missing date range"))

  meteo <- if (row$date_from == row$date_to) {
    rNDC::adc_weather_date(closest_id, row$date_from, adc_token, save_params = retrieve_params_path(stem))
  } else {
    rNDC::adc_weather_long_period(station = closest_id, trange = c(row$date_from, row$date_to), token = adc_token,
                                  by_days = weather_chunk_days, sleep_sec = weather_chunk_sleep,
                                  save_params = retrieve_params_path(stem))
  }
  if (is.null(meteo) || nrow(meteo) == 0) return(failed("no data returned"))

  retrieve_outcome_data(meteo, "Weather", ext,
               ok_msg = "Retrieved: Weather",
               fail_msg = "Failed: Weather - could not write output")
}

# A raster dataset (land use, nitrogen): the result of the rNDC function is a list with a `stack`
retrieve_raster <- function(dataset, year, retrieve) {
  res <- retrieve()
  if (is.null(res) || is.null(res$stack) || terra::nlyr(res$stack) == 0) {
    return(retrieve_outcome_none(paste0("Failed: ", dataset, " - no raster returned for year ", year), "tif"))
  }
  retrieve_outcome_data(res$stack, dataset, "tif",
               ok_msg = paste0("Retrieved: ", dataset, " raster for ", if (dataset == "Land Use") "year " else "", year),
               fail_msg = paste0("Failed: ", dataset, " - could not write output"))
}

retrieve_nitrogen <- function(row, ndc_token, stem = NULL) {
  retrieve_raster("Nitrogen", row$year, function() {
    rNDC::ndc_nitrogen_raster(roi = row$polygon_sf[[1]], year = row$year, layers = nitrogen_layer_choices,
                              token = ndc_token, output_dir = tempdir(), overwrite = TRUE, limit = 100,
                              file_prefix = tempfile(), save_params = retrieve_params_path(stem))
  })
}

retrieve_land_use <- function(row, ndc_token, stem = NULL) {
  year <- row$year
  if (is.na(year)) year <- as.integer(landuse_default_year)
  retrieve_raster("Land Use", year, function() {
    rNDC::ndc_landuse_raster(roi = row$polygon_sf[[1]], year = year, token = ndc_token, output_dir = tempdir(),
                             overwrite = TRUE, limit = 100, file_prefix = tempfile(), save_params = retrieve_params_path(stem))
  })
}

# Monthly NDVI statistics for LTER and SNL project areas, from the STAC API
retrieve_ndvi_statistics <- function(row, stem = NULL) {
  stats <- project_ndvi_stats(row$polygon_sf[[1]], project_ndvi_collection(row$polygon),
                                    row$date_from, row$date_to, save_params = retrieve_params_path(stem))
  if (identical(stats$status, "skip")) {
    msg <- if (is.null(stats$error)) "NDVI statistics unavailable for this area." else stats$error
    return(retrieve_outcome_none(paste0("Skipped: NDVI statistics - ", msg), "csv", status = msg))
  }
  if (identical(stats$status, "error")) {
    msg <- if (is.null(stats$error)) "request failed" else stats$error
    return(retrieve_outcome_none(paste0("Failed: NDVI statistics - ", msg), "csv"))
  }
  if (identical(stats$status, "empty") || is.null(stats$data) || nrow(stats$data) == 0) {
    msg <- "No NDVI statistics available for this area/period"
    return(retrieve_outcome_none(paste0("NDVI statistics: ", msg), "csv", status = msg))
  }

  # Tag with the polygon label so multiple areas are distinguishable.
  data <- cbind(polygon = row$polygon, stats$data)
  retrieve_outcome_data(data, "NDVI_stats", "csv",
               ok_msg = paste0("Retrieved: NDVI statistics (", nrow(data), " monthly rows)"),
               fail_msg = "Failed: NDVI statistics - could not write output",
               writer = function(path) {
                 tryCatch({ utils::write.csv(data, path, row.names = FALSE); TRUE }, error = function(e) FALSE)
               })
}

# Monthly average NDVI rasters (one month, or a range of months), from GroenMonitor. The period is the one
# stored in the overview row, not what the input widgets say at the time of the retrieval.
retrieve_ndvi_rasters <- function(row, index, stem = NULL) {
  poly <- row$polygon_sf[[1]]
  from <- row$date_from
  to <- row$date_to

  if (!is.na(from) && !is.na(to) && format(from, "%Y-%m") == format(to, "%Y-%m")) {
    year <- as.integer(format(from, "%Y"))
    month <- as.integer(format(from, "%m"))
    label <- paste0(year, "-", sprintf("%02d", month))
    r <- rNDC::gm_ndvi_month(poly, year, month, save_params = retrieve_params_path(stem))
    if (is.null(r)) {
      msg <- "No data available for this month"
      return(retrieve_outcome_none(paste0("NDVI ", label, ": ", msg), "tif", status = msg))
    }
    return(retrieve_outcome_data(r, "NDVI", "tif",
                        ok_msg = paste0("Retrieved: NDVI ", label),
                        fail_msg = paste0("Failed: NDVI ", label, " - could not write output")))
  }

  if (is.na(from) || is.na(to)) {
    return(retrieve_outcome_none(paste0("Failed: NDVI - invalid date range for row ", index), "tif"))
  }

  err <- NULL
  stack <- tryCatch(
    rNDC::gm_ndvi_stack(roi = poly, trange = c(from, to), save_params = retrieve_params_path(stem)),
    error = function(e) { err <<- conditionMessage(e); NULL }
  )
  if (is.null(stack) || terra::nlyr(stack) == 0) {
    msg <- if (!is.null(err)) paste0("Failed: NDVI - ", err) else "No data available for this period"
    return(retrieve_outcome_none(paste0("NDVI - ", msg, " (", from, " to ", to, ")"), "tif", status = msg))
  }

  names(stack) <- sub("^ndvi_mean_", "NDVI_", names(stack))
  period <- paste0(format(from, "%Y-%m"), " to ", format(to, "%Y-%m"))
  retrieve_outcome_data(stack, "NDVI", "tif",
               ok_msg = paste0("Retrieved: NDVI stack ", period),
               fail_msg = "Failed: NDVI - could not write output")
}

retrieve_ndvi <- function(row, index, stem = NULL) {
  if (tolower(as.character(row$view)) == "statistics") retrieve_ndvi_statistics(row, stem) else retrieve_ndvi_rasters(row, index, stem)
}

# Call the retrieval function of the dataset of `row`
retrieve_dataset <- function(row, index, ndc_token, adc_token, stem = NULL) {
  switch(row$dataset,
    "Agricultural fields" = retrieve_agricultural_fields(row, adc_token, stem),
    "AHN" = retrieve_ahn(row, adc_token, stem),
    "Soil map" = retrieve_soil_map(row, adc_token, stem),
    "Weather" = retrieve_weather(row, adc_token, stem),
    "Nitrogen" = retrieve_nitrogen(row, ndc_token, stem),
    "Land Use" = retrieve_land_use(row, ndc_token, stem),
    "NDVI" = retrieve_ndvi(row, index, stem),
    retrieve_outcome_none(paste0("Skipped: ", row$dataset, " is not wired to a retrieval endpoint yet."),
                 NA_character_, status = "skipped")
  )
}
