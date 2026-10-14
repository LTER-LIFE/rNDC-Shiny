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

# The retrieval functions of rNDC can record their parameters (`save_params`, see rNDC::ndc_repeat()). A retrieval
# records them in files that start with `stem` (NULL: not at all): `params_slot(stem)` is the path for the file of the
# data, `params_slot(stem, "station")` for another call of the same dataset. Afterwards the files are moved to their
# place in the download, or read (see `retrieve_row()`).
params_slot <- function(stem, slot = "data") if (is.null(stem)) NULL else paste0(stem, "_", slot, ".json")

# File type of the vector datasets: a table for the Statistics view, a GeoPackage otherwise.
vector_ext <- function(row) if (tolower(as.character(row$view)) == "statistics") "csv" else "gpkg"

# AgroDataCube answers with GeoJSON
adc_sf_outcome <- function(row, response, geometry = list(tolerance = 0)) {
  data <- geojsonsf::geojson_sf(jsonlite::toJSON(response, auto_unbox = TRUE))
  outcome_data(data, row$dataset, vector_ext(row),
               ok_msg = paste0("Retrieved: ", row$dataset, simplified_note(geometry)),
               fail_msg = paste0("Failed: ", row$dataset, " - could not write output"))
}

retrieve_agricultural_fields <- function(row, adc_token, stem = NULL) {
  geometry <- adc_geometry(row)
  response <- rNDC::adc_get_all("Fields", c(geometry = geometry$wkt, epsg = "4326", year = row$year, output_epsg = "4326"),
                                token = adc_token, save_params = params_slot(stem))
  adc_sf_outcome(row, response, geometry)
}

retrieve_ahn <- function(row, adc_token, stem = NULL) {
  geometry <- adc_geometry(row)
  url <- rNDC::adc_url("AHN", params = c(geometry = geometry$wkt, epsg = "4326"))
  adc_sf_outcome(row, rNDC::adc_get(url = url, token = adc_token, save_params = params_slot(stem)), geometry)
}

retrieve_soil_map <- function(row, adc_token, stem = NULL) {
  geometry <- adc_geometry(row)
  response <- rNDC::adc_get_all("Soiltypes", c(geometry = geometry$wkt, epsg = "4326", output_epsg = "4326"),
                                token = adc_token, save_params = params_slot(stem))
  adc_sf_outcome(row, response, geometry)
}

retrieve_weather <- function(row, adc_token, stem = NULL) {
  ext <- vector_ext(row)
  failed <- function(message) outcome_none(paste0("Failed: Weather - ", message), ext)

  closest_id <- rNDC::adc_weather_station(row$wkt, token = adc_token,
                                          save_params = params_slot(stem, "station"))$closest_id
  # adc_weather_station() gives character(0) (not NULL) when no id is found
  if (length(closest_id) != 1 || is.na(closest_id) || !nzchar(closest_id)) {
    return(failed("no nearby station found"))
  }
  if (is.na(row$date_from) || is.na(row$date_to)) return(failed("missing date range"))

  meteo <- if (row$date_from == row$date_to) {
    rNDC::adc_weather_date(closest_id, row$date_from, adc_token, save_params = params_slot(stem))
  } else {
    rNDC::adc_weather_long_period(station = closest_id, trange = c(row$date_from, row$date_to), token = adc_token,
                                  by_days = weather_chunk_days, sleep_sec = weather_chunk_sleep,
                                  save_params = params_slot(stem))
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

retrieve_nitrogen <- function(row, ndc_token, stem = NULL) {
  retrieve_raster("Nitrogen", row$year, function() {
    rNDC::ndc_nitrogen_raster(roi = row$polygon_sf[[1]], year = row$year, layers = nitrogen_layer_choices,
                              token = ndc_token, output_dir = tempdir(), overwrite = TRUE, limit = 100,
                              file_prefix = tempfile(), save_params = params_slot(stem))
  })
}

retrieve_land_use <- function(row, ndc_token, stem = NULL) {
  year <- row$year
  if (is.na(year)) year <- as.integer(landuse_default_year)
  retrieve_raster("Land Use", year, function() {
    rNDC::ndc_landuse_raster(roi = row$polygon_sf[[1]], year = year, token = ndc_token, output_dir = tempdir(),
                             overwrite = TRUE, limit = 100, file_prefix = tempfile(), save_params = params_slot(stem))
  })
}

# Monthly NDVI statistics for LTER and SNL project areas, from the STAC API
retrieve_ndvi_statistics <- function(row, stem = NULL) {
  stats <- fetch_ndvi_stats_monthly(row$polygon_sf[[1]], detect_ndvi_collection(row$polygon),
                                    row$date_from, row$date_to, save_params = params_slot(stem))
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
retrieve_ndvi_rasters <- function(row, index, stem = NULL) {
  poly <- row$polygon_sf[[1]]
  from <- row$date_from
  to <- row$date_to

  if (!is.na(from) && !is.na(to) && format(from, "%Y-%m") == format(to, "%Y-%m")) {
    year <- as.integer(format(from, "%Y"))
    month <- as.integer(format(from, "%m"))
    label <- paste0(year, "-", sprintf("%02d", month))
    r <- rNDC::gm_ndvi_month(poly, year, month, save_params = params_slot(stem))
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
    rNDC::gm_ndvi_stack(roi = poly, trange = c(from, to), save_params = params_slot(stem)),
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
    outcome_none(paste0("Skipped: ", row$dataset, " is not wired to a retrieval endpoint yet."),
                 NA_character_, status = "skipped")
  )
}

# ---- One row of the overview ----

# Retrieve the dataset of one row of the overview. With a `workdir`, the data and the polygon are written
# there; without one, nothing is written. Returns the retrieved `data` and its `name` (NULL if nothing was
# retrieved), the rows for the download summary (`manifest`) and the `messages`. With `parameters`, the rNDC functions
# record their parameters (see rNDC::ndc_repeat()): with a `workdir` they go to the folder `parameters` of it, named
# after the data file; without one they are returned in `parameters` (the texts of the JSON files, by the call:
# `data`, and for Weather `station`).
retrieve_row <- function(row, index, workdir = NULL, ndc_token, adc_token, parameters = TRUE) {
  save_files <- !is.null(workdir)
  ds <- row$dataset
  view <- row$view
  polygon <- row$polygon
  date_label <- format_date_label(row$year, row$date_from, row$date_to)
  summary_row <- function(file_type, file_path, status, note = "", parameters = NA_character_) {
    manifest_row(ds, view, polygon, date_label, file_type, file_path, status, note, parameters = parameters)
  }

  # the files that the retrieval functions record their parameters in start with `stem` (hidden, in the folder of the
  # download, or in a folder of their own); they are moved or read below, and what is left is removed
  stem <- NULL
  recorded <- list()
  if (isTRUE(parameters)) {
    scratch <- if (save_files) workdir else tempfile("ndc_params_")
    dir.create(scratch, showWarnings = FALSE, recursive = TRUE)
    stem <- file.path(scratch, paste0(".params_", index))
    on.exit({
      unlink(parameter_files(stem))
      if (!save_files) unlink(scratch, recursive = TRUE)
    }, add = TRUE)
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
    outcome <- retrieve_dataset(row, index, ndc_token, adc_token, stem)

    if (is.null(outcome$data)) {
      messages <- c(messages, outcome$message)
      manifest <- c(manifest, list(summary_row(outcome$file_type, NA_character_, outcome$status)))
    } else {
      data <- outcome$data
      name <- paste0(outcome$name, "_", index)
      if (save_files) {
        file <- unique_export_file(workdir, file_base, outcome$ext, date_label)
        ok <- outcome$writer(file)
        placed <- if (ok && !is.null(stem)) place_parameters(stem, workdir, file) else NA_character_
        manifest <- c(manifest, list(summary_row(outcome$ext, basename(file), if (ok) "ok" else "failed",
                                                 parameters = placed)))
        messages <- c(messages, if (ok) outcome$ok_msg else outcome$fail_msg)
      } else {
        if (!is.null(stem)) recorded <- read_parameters(stem)
        manifest <- c(manifest, list(summary_row(outcome$ext, NA_character_, "ok")))
        messages <- c(messages, outcome$ok_msg)
      }
    }
  }, error = function(e) {
    messages <<- c(messages, paste0("Failed: ", ds, " - ", e$message))
    manifest <<- c(manifest, list(summary_row(NA_character_, NA_character_, "failed", note = e$message)))
  })

  list(data = data, name = name, manifest = manifest, messages = messages, parameters = recorded)
}

# ---- The parameters of the retrievals (JSON files, see rNDC::ndc_repeat()) ----

# The files that were recorded for the stem
parameter_files <- function(stem) {
  files <- list.files(dirname(stem), all.files = TRUE, full.names = TRUE)
  files[startsWith(basename(files), paste0(basename(stem), "_")) & endsWith(files, ".json")]
}

# The slot (`data`, `station`) of a recorded file
parameter_slot <- function(stem, file) {
  sub("\\.json$", "", substring(basename(file), nchar(basename(stem)) + 2))
}

# Move the files recorded for the stem to the folder `parameters` of the download, named after `data_file`: the file
# of the data is `<name>.json`, another one `<name>_<slot>.json`. Returns the path of the file of the data in the zip
# (`parameters/<name>.json`), or NA if the retrieval recorded none.
place_parameters <- function(stem, workdir, data_file) {
  files <- parameter_files(stem)
  if (length(files) == 0) return(NA_character_)
  dir.create(file.path(workdir, "parameters"), showWarnings = FALSE)
  base <- tools::file_path_sans_ext(basename(data_file))
  placed <- NA_character_
  for (file in files) {
    slot <- parameter_slot(stem, file)
    name <- if (slot == "data") base else paste0(base, "_", slot)
    target <- file.path(workdir, "parameters", paste0(name, ".json"))
    if (file.exists(target)) next  # (the names of the data files are free: this does not happen)
    if (file.rename(file, target) && slot == "data") placed <- paste0("parameters/", name, ".json")
  }
  placed
}

# The texts of the files recorded for the stem, by slot
read_parameters <- function(stem) {
  files <- parameter_files(stem)
  texts <- lapply(files, function(file) paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  stats::setNames(texts, vapply(files, function(file) parameter_slot(stem, file), ""))
}

# Path for the file of a dataset in `dir`. The names are built from the dataset, the view and the polygon,
# so rows that differ only in year or period would overwrite each other: a row whose name is already taken
# gets its period appended (`..._2023`), or a number if that is taken too.
unique_export_file <- function(dir, base, ext, date_label) {
  file <- file.path(dir, paste0(base, ".", ext))
  if (!file.exists(file)) return(file)

  period <- tolower(safe_filename(gsub(" - ", "_to_", date_label)))
  candidates <- c(if (nzchar(period)) paste0(base, "_", period), paste0(base, "_", 2:999))
  for (candidate in candidates) {
    file <- file.path(dir, paste0(candidate, ".", ext))
    if (!file.exists(file)) return(file)
  }
  stop("Could not find a free file name for ", base, ".", ext, call. = FALSE)
}

# Export the selected polygon as a GeoPackage, once per polygon. In the download summary this row is
# labelled as the reference polygon (dataset "Area") rather than the dataset it was exported alongside.
export_reference_polygon <- function(row, workdir, polygon_name, date_label) {
  geom <- sf::st_sf(polygon = as.character(row$polygon), geometry = sf::st_geometry(row$polygon_sf[[1]]))
  if (is.na(sf::st_crs(geom))) sf::st_crs(geom) <- 4326

  file <- reference_polygon_file(workdir, polygon_name, geom)
  if (is.null(file)) return(NULL)  # this polygon was exported already

  ok <- write_sf_safe(geom, file)
  manifest_row("Area", "Reference polygon", row$polygon, date_label, "gpkg",
               if (ok) basename(file) else NA_character_,
               if (ok) "ok" else "failed", note = "selected polygon geometry")
}

# Path for the file of the polygon `geom` in `dir`: NULL if the same polygon is already there. Polygons are
# named after the polygon, and different polygons can share a name (e.g. uploaded files with the same file
# name), so a name taken by another polygon gets a number appended.
reference_polygon_file <- function(dir, name, geom) {
  for (candidate in c(name, paste0(name, "_", 2:999))) {
    file <- file.path(dir, paste0(candidate, ".gpkg"))
    if (!file.exists(file)) return(file)
    if (same_polygon(file, geom)) return(NULL)
  }
  stop("Could not find a free file name for ", name, ".gpkg", call. = FALSE)
}

# Does the GeoPackage `file` hold the same geometry as `geom`?
same_polygon <- function(file, geom) {
  existing <- tryCatch(sf::st_read(file, quiet = TRUE), error = function(e) NULL)
  if (is.null(existing) || nrow(existing) != nrow(geom)) return(FALSE)
  if (sf::st_crs(existing) != sf::st_crs(geom)) existing <- sf::st_transform(existing, sf::st_crs(geom))
  same <- sf::st_equals(sf::st_geometry(existing), sf::st_geometry(geom), sparse = FALSE)
  all(diag(same))
}

# ---- Download summary, files and zip ----

# A row of the download summary
manifest_row <- function(dataset, view, polygon, date, file_type, file_path, status, note = "",
                         reference = NA_character_, license = NA_character_, parameters = NA_character_) {
  tibble::tibble(dataset = dataset, view = view, polygon = polygon, date = date, file_type = file_type,
                 file_path = file_path, status = status, note = note, reference = reference, license = license,
                 parameters = parameters)
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
  # mirror: the files in folders (`parameters`) keep their folder; the default puts every file at the top
  if (length(files) > 0) zip::zipr(zipfile, files = files, root = workdir, mode = "mirror")
  invisible(zipfile)
}

# Write a retrieved object to `outfile`, in the format of its extension. Returns TRUE if it was written.
write_sf_safe <- function(obj, outfile) {
  if (is.null(obj)) return(FALSE)

  ext <- tolower(tools::file_ext(outfile))

  if (ext == "csv") {
    # tables: the attributes of an sf object, or a data frame
    df <- if (inherits(obj, "sf")) sf::st_drop_geometry(obj) else if (inherits(obj, "data.frame")) obj else NULL

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
