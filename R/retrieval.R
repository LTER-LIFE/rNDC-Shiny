# Retrieval of the datasets in the overview.
#
# `retrieve_row()` handles one row of the overview: it calls the retrieval function of the dataset
# (`retrieve_<dataset>()`), writes the file and describes the outcome for the download summary and the
# messages panel. Each `retrieve_<dataset>()` takes the row (a one-row tibble with the columns of the
# overview) and returns an "outcome", made with `outcome_data()` or `outcome_none()`.

# ---- Outcomes ----

# The dataset was retrieved. `name` is the prefix of its name in the returned list (the row number is
# added), `ext` the file extension, `ok_msg` / `fail_msg` the messages for a retrieved dataset / a file that
# could not be written, and `writer` the function that writes it (`function(path)`, TRUE if it worked).
outcome_data <- function(data, name, ext, ok_msg, fail_msg, writer = NULL) {
  if (is.null(writer)) writer <- function(path) write_sf_safe(data, path)
  list(data = data, name = name, ext = ext, ok_msg = ok_msg, fail_msg = fail_msg, writer = writer)
}

# Nothing was retrieved: `message` for the messages panel, `file_type` and `status` for the download
# summary.
outcome_none <- function(message, file_type, status = "failed") {
  list(data = NULL, message = message, file_type = file_type, status = status)
}

# ---- Retrieval per dataset ----

# File type of the vector datasets: a table for the Statistics view, a GeoPackage otherwise.
vector_ext <- function(row) if (tolower(as.character(row$view)) == "statistics") "csv" else "gpkg"

# AgroDataCube answers with GeoJSON
adc_sf_outcome <- function(row, response) {
  data <- geojsonsf::geojson_sf(jsonlite::toJSON(response, auto_unbox = TRUE))
  outcome_data(data, row$dataset, vector_ext(row),
               ok_msg = paste0("Retrieved: ", row$dataset),
               fail_msg = paste0("Failed: ", row$dataset, " - could not write output"))
}

retrieve_agricultural_fields <- function(row, adc_token) {
  response <- adc_get_all("Fields", c(geometry = row$wkt, epsg = "4326", year = row$year, output_epsg = "4326"),
                          token = adc_token)
  adc_sf_outcome(row, response)
}

retrieve_ahn <- function(row, adc_token) {
  url <- rNDC::adc_url("AHN", params = c(geometry = row$wkt, epsg = "4326"))
  adc_sf_outcome(row, rNDC::adc_get(url = url, token = adc_token))
}

retrieve_soil_map <- function(row, adc_token) {
  response <- adc_get_all("Soiltypes", c(geometry = row$wkt, epsg = "4326", output_epsg = "4326"),
                          token = adc_token)
  adc_sf_outcome(row, response)
}

retrieve_weather <- function(row, adc_token) {
  ext <- vector_ext(row)
  failed <- function(message) outcome_none(paste0("Failed: Weather - ", message), ext)

  closest_id <- rNDC::get_closest_meteostation(row$wkt, token = adc_token)$closest_id
  # get_closest_meteostation() gives character(0) (not NULL) when no id is found
  if (length(closest_id) != 1 || is.na(closest_id) || !nzchar(closest_id)) {
    return(failed("no nearby station found"))
  }
  if (is.na(row$date_from) || is.na(row$date_to)) return(failed("missing date range"))

  meteo <- if (row$date_from == row$date_to) {
    rNDC::get_meteo_for_date(closest_id, row$date_from, adc_token)
  } else {
    rNDC::get_meteo_for_long_period(meteostation = closest_id, fromdate = row$date_from,
                                    todate = row$date_to, token = adc_token, by_days = 200, sleep_sec = 0.5)
  }
  if (is.null(meteo) || nrow(meteo) == 0) return(failed("no data returned"))

  outcome_data(meteo, "Weather", ext,
               ok_msg = "Retrieved: Weather",
               fail_msg = "Failed: Weather - could not write output")
}

# A raster dataset (land use, nitrogen): the result of the rNDC function is a list with a `stack`
retrieve_raster <- function(dataset, year, retrieve) {
  res <- retrieve()
  if (is.null(res) || is.null(res$stack) || terra::nlyr(res$stack) == 0) {
    return(outcome_none(paste0("Failed: ", dataset, " - no raster returned for year ", year), "tif"))
  }
  outcome_data(res$stack, dataset, "tif",
               ok_msg = paste0("Retrieved: ", dataset, " raster for ", if (dataset == "Land Use") "year " else "", year),
               fail_msg = paste0("Failed: ", dataset, " - could not write output"))
}

retrieve_nitrogen <- function(row, ndc_token) {
  retrieve_raster("Nitrogen", row$year, function() {
    rNDC::get_nitrogen_raster(aoi = row$polygon_sf[[1]], year = row$year, layers = nitrogen_layer_choices,
                              token = ndc_token, out_dir = tempdir(), overwrite = TRUE, limit = 100,
                              file_prefix = tempfile())
  })
}

retrieve_land_use <- function(row, ndc_token) {
  year <- row$year
  if (is.na(year)) year <- as.integer(landuse_default_year)
  retrieve_raster("Land Use", year, function() {
    rNDC::get_landuse_raster(aoi = row$polygon_sf[[1]], year = year, token = ndc_token, out_dir = tempdir(),
                             overwrite = TRUE, limit = 100, file_prefix = tempfile())
  })
}

# Monthly NDVI statistics for LTER and SNL project areas, from the STAC API
retrieve_ndvi_statistics <- function(row) {
  stats <- fetch_ndvi_stats_monthly(row$polygon_sf[[1]], detect_ndvi_collection(row$polygon),
                                    row$date_from, row$date_to)
  if (identical(stats$status, "skip")) {
    msg <- if (is.null(stats$error)) "NDVI statistics unavailable for this area." else stats$error
    return(outcome_none(paste0("Skipped: NDVI statistics - ", msg), "csv", status = msg))
  }
  if (identical(stats$status, "error")) {
    msg <- if (is.null(stats$error)) "request failed" else stats$error
    return(outcome_none(paste0("Failed: NDVI statistics - ", msg), "csv"))
  }
  if (identical(stats$status, "empty") || is.null(stats$data) || nrow(stats$data) == 0) {
    msg <- "No NDVI statistics available for this area/period"
    return(outcome_none(paste0("NDVI statistics: ", msg), "csv", status = msg))
  }

  # Tag with the polygon label so multiple areas are distinguishable.
  data <- cbind(polygon = row$polygon, stats$data)
  outcome_data(data, "NDVI_stats", "csv",
               ok_msg = paste0("Retrieved: NDVI statistics (", nrow(data), " monthly rows)"),
               fail_msg = "Failed: NDVI statistics - could not write output",
               writer = function(path) {
                 tryCatch({ utils::write.csv(data, path, row.names = FALSE); TRUE }, error = function(e) FALSE)
               })
}

# Monthly average NDVI rasters (one month, or a range of months), from GroenMonitor. The period is the one
# stored in the overview row, not what the input widgets say at the time of the retrieval.
retrieve_ndvi_rasters <- function(row, index) {
  poly <- row$polygon_sf[[1]]
  from <- row$date_from
  to <- row$date_to

  if (!is.na(from) && !is.na(to) && format(from, "%Y-%m") == format(to, "%Y-%m")) {
    year <- as.integer(format(from, "%Y"))
    month <- as.integer(format(from, "%m"))
    label <- paste0(year, "-", sprintf("%02d", month))
    r <- rNDC::download_avg_ndvi_month(poly, year, month)
    if (is.null(r)) {
      msg <- "No data available for this month"
      return(outcome_none(paste0("NDVI ", label, ": ", msg), "tif", status = msg))
    }
    return(outcome_data(r, "NDVI", "tif",
                        ok_msg = paste0("Retrieved: NDVI ", label),
                        fail_msg = paste0("Failed: NDVI ", label, " - could not write output")))
  }

  if (is.na(from) || is.na(to)) {
    return(outcome_none(paste0("Failed: NDVI - invalid date range for row ", index), "tif"))
  }

  err <- NULL
  stack <- tryCatch(
    rNDC::download_avg_ndvi_stack(poly = poly,
                                  start_year = as.integer(format(from, "%Y")), start_month = as.integer(format(from, "%m")),
                                  end_year = as.integer(format(to, "%Y")), end_month = as.integer(format(to, "%m"))),
    error = function(e) { err <<- conditionMessage(e); NULL }
  )
  if (is.null(stack) || terra::nlyr(stack) == 0) {
    msg <- if (!is.null(err)) paste0("Failed: NDVI - ", err) else "No data available for this period"
    return(outcome_none(paste0("NDVI - ", msg, " (", from, " to ", to, ")"), "tif", status = msg))
  }

  names(stack) <- sub("^ndvi_mean_", "NDVI_", names(stack))
  period <- paste0(format(from, "%Y-%m"), " to ", format(to, "%Y-%m"))
  outcome_data(stack, "NDVI", "tif",
               ok_msg = paste0("Retrieved: NDVI stack ", period),
               fail_msg = "Failed: NDVI - could not write output")
}

retrieve_ndvi <- function(row, index) {
  if (tolower(as.character(row$view)) == "statistics") retrieve_ndvi_statistics(row) else retrieve_ndvi_rasters(row, index)
}

# Call the retrieval function of the dataset of `row`
retrieve_dataset <- function(row, index, ndc_token, adc_token) {
  switch(row$dataset,
    "Agricultural fields" = retrieve_agricultural_fields(row, adc_token),
    "AHN" = retrieve_ahn(row, adc_token),
    "Soil map" = retrieve_soil_map(row, adc_token),
    "Weather" = retrieve_weather(row, adc_token),
    "Nitrogen" = retrieve_nitrogen(row, ndc_token),
    "Land Use" = retrieve_land_use(row, ndc_token),
    "NDVI" = retrieve_ndvi(row, index),
    outcome_none(paste0("Skipped: ", row$dataset, " is not wired to a retrieval endpoint yet."),
                 NA_character_, status = "skipped")
  )
}

# ---- One row of the overview ----

# Retrieve the dataset of one row of the overview. With a `workdir`, the data and the polygon are written
# there; without one, nothing is written. Returns the retrieved `data` and its `name` (NULL if nothing was
# retrieved), the rows for the download summary (`manifest`) and the `messages`.
retrieve_row <- function(row, index, workdir = NULL, ndc_token, adc_token) {
  save_files <- !is.null(workdir)
  ds <- row$dataset
  view <- row$view
  polygon <- row$polygon
  date_label <- format_date_label(row$year, row$date_from, row$date_to)
  summary_row <- function(file_type, file_path, status, note = "") {
    manifest_row(ds, view, polygon, date_label, file_type, file_path, status, note)
  }

  # Filename base includes the polygon name; all output filenames lowercase.
  polygon_name <- tolower(safe_filename(as.character(polygon)))
  file_base <- tolower(safe_filename(paste0(ds, "_", view, "_", polygon_name)))

  manifest <- list()
  messages <- character(0)
  data <- NULL
  name <- NULL

  if (save_files && !is.null(row$polygon_sf[[1]])) {
    polygon_row <- tryCatch(export_reference_polygon(row, workdir, polygon_name, date_label), error = function(e) NULL)
    if (!is.null(polygon_row)) manifest <- c(manifest, list(polygon_row))
  }

  tryCatch({
    outcome <- retrieve_dataset(row, index, ndc_token, adc_token)

    if (is.null(outcome$data)) {
      messages <- c(messages, outcome$message)
      manifest <- c(manifest, list(summary_row(outcome$file_type, NA_character_, outcome$status)))
    } else {
      data <- outcome$data
      name <- paste0(outcome$name, "_", index)
      if (save_files) {
        file <- file.path(workdir, paste0(file_base, ".", outcome$ext))
        ok <- outcome$writer(file)
        manifest <- c(manifest, list(summary_row(outcome$ext, basename(file), if (ok) "ok" else "failed")))
        messages <- c(messages, if (ok) outcome$ok_msg else outcome$fail_msg)
      } else {
        manifest <- c(manifest, list(summary_row(outcome$ext, NA_character_, "ok")))
        messages <- c(messages, outcome$ok_msg)
      }
    }
  }, error = function(e) {
    messages <<- c(messages, paste0("Failed: ", ds, " - ", e$message))
    manifest <<- c(manifest, list(summary_row(NA_character_, NA_character_, "failed", note = e$message)))
  })

  list(data = data, name = name, manifest = manifest, messages = messages)
}

# Export the selected polygon as a GeoPackage, once per polygon. In the download summary this row is
# labelled as the reference polygon (dataset "Area") rather than the dataset it was exported alongside.
export_reference_polygon <- function(row, workdir, polygon_name, date_label) {
  file <- file.path(workdir, paste0(polygon_name, ".gpkg"))
  if (file.exists(file)) return(NULL)

  geom <- sf::st_sf(polygon = as.character(row$polygon), geometry = sf::st_geometry(row$polygon_sf[[1]]))
  if (is.na(sf::st_crs(geom))) sf::st_crs(geom) <- 4326
  ok <- write_sf_safe(geom, file)
  manifest_row("Area", "Reference polygon", row$polygon, date_label, "gpkg",
               if (ok) basename(file) else NA_character_,
               if (ok) "ok" else "failed", note = "selected polygon geometry")
}

# ---- Download summary, files and zip ----

# A row of the download summary
manifest_row <- function(dataset, view, polygon, date, file_type, file_path, status, note = "",
                         reference = NA_character_, license = NA_character_) {
  tibble::tibble(dataset = dataset, view = view, polygon = polygon, date = date, file_type = file_type,
                 file_path = file_path, status = status, note = note, reference = reference, license = license)
}

format_date_label <- function(year, date_from, date_to) {
  if (!is.na(year)) return(as.character(year))

  if (!is.na(date_from) && !is.na(date_to)) {
    if (date_from == date_to) return(format(date_from, "%Y-%m-%d"))
    return(paste0(format(date_from, "%Y-%m-%d"), " - ", format(date_to, "%Y-%m-%d")))
  }

  if (!is.na(date_from)) return(format(date_from, "%Y-%m-%d"))
  if (!is.na(date_to)) return(format(date_to, "%Y-%m-%d"))
  ""
}

# Did we retrieve any data? A row counts if its status is "ok" and it isn't the polygon-geometry helper
# file. No file path is required, so this also works when nothing is written.
any_data_produced <- function(manifest) {
  if (nrow(manifest) == 0 || !"status" %in% names(manifest)) return(FALSE)
  is_ok <- !is.na(manifest$status) & manifest$status == "ok"
  not_polygon <- if ("note" %in% names(manifest)) {
    is.na(manifest$note) | manifest$note != "selected polygon geometry"
  } else TRUE
  any(is_ok & not_polygon)
}

# `root` avoids setwd(), which would change the working directory of every session served by this R process.
zip_export <- function(workdir, zipfile) {
  files <- list.files(workdir, recursive = TRUE, full.names = FALSE, no.. = TRUE)
  if (length(files) > 0) zip::zipr(zipfile, files = files, root = workdir)
  invisible(zipfile)
}

# Write a retrieved object to `outfile`, in the format of its extension. Returns TRUE if it was written.
write_sf_safe <- function(obj, outfile) {
  if (is.null(obj)) return(FALSE)

  ext <- tolower(tools::file_ext(outfile))

  if (ext == "csv") {
    df <- if (inherits(obj, "sf")) {
      sf::st_drop_geometry(obj)
    } else if (inherits(obj, "data.frame")) {
      obj
    } else if (inherits(obj, "SpatRaster")) {
      as.data.frame(obj, xy = TRUE, na.rm = FALSE)
    } else if (inherits(obj, "SpatVector")) {
      as.data.frame(obj)
    } else {
      tryCatch(as.data.frame(obj), error = function(e) NULL)
    }

    if (is.null(df) || nrow(df) == 0) return(FALSE)
    utils::write.csv(df, outfile, row.names = FALSE)
    return(TRUE)
  }

  if (ext == "gpkg") {
    if (inherits(obj, "sf")) {
      sf::st_write(obj, outfile, delete_dsn = TRUE, quiet = TRUE)
      return(TRUE)
    }

    if (inherits(obj, "SpatVector")) {
      terra::writeVector(obj, outfile, overwrite = TRUE)
      return(TRUE)
    }

    return(FALSE)
  }

  if (ext %in% c("tif", "tiff")) {
    if (inherits(obj, "SpatRaster")) {
      terra::writeRaster(obj, outfile, overwrite = TRUE)
      return(TRUE)
    }
    return(FALSE)
  }

  FALSE
}
