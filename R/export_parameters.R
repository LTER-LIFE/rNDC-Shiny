# The parameters of the datasets of the overview as files for rNDC::ndc_repeat(), without retrieving anything: the files that a
# retrieval records (see R/export_files.R), written from the rows of the overview by rNDC::ndc_params_write().

# The calls that retrieve the dataset of an overview row, as a list of list(slot, fun, args): `fun` is the rNDC function, `args` the
# arguments that its file records, and `slot` "data" or, for the weather station that Weather asks for first, "station". These are
# the calls of the retrieve_<dataset>() functions of R/retrieve_dataset.R (the tests compare them). `station` is the id of the weather
# station: Weather has the call for its data only once it is known (the retrieval asks the AgroDataCube for it).
row_calls <- function(row, station = NULL) {
  poly <- row$polygon_sf[[1]]
  view <- tolower(as.character(row$view))
  from <- row$date_from
  to <- row$date_to
  call <- function(fun, args, slot = "data") list(slot = slot, fun = fun, args = args)

  switch(as.character(row$dataset),
    "Agricultural fields" = list(call("adc_get_all", list(
      option = "Fields",
      params = c(geometry = adc_geometry(row)$wkt, epsg = "4326", year = row$year, output_epsg = "4326")))),

    "AHN" = list(if (view == "statistics") call("ndc_ahn_stats", list(roi = poly)) else call("ndc_ahn_raster", list(roi = poly, limit = 100))),

    "Soil map" = list(call("ndc_soil_raster", list(roi = poly, limit = 100))),

    "Nitrogen" = list(call("ndc_nitrogen_raster", list(roi = poly, year = as.character(row$year), layers = nitrogen_layer_choices,
                                                      limit = 100))),

    "Land Use" = list(call("ndc_landuse_raster", list(
      roi = poly, year = if (is.na(row$year)) as.integer(landuse_default_year) else row$year, limit = 100))),

    "NDVI" = if (view == "statistics") {
      collection <- project_ndvi_collection(row$polygon)
      if (is.na(collection)) return(list())          # NDVI statistics exist for the project areas only
      ids <- if ("ndc_id" %in% names(poly)) unique(as.character(poly$ndc_id[!is.na(poly$ndc_id)])) else character(0)
      list(call("ndc_ndvi_stats", list(roi = poly, collection = collection, trange = c(from, to),
                                       ndc_id = if (length(ids) > 0) ids else NULL)))
    } else if (!is.na(from) && !is.na(to) && format(from, "%Y-%m") == format(to, "%Y-%m")) {
      list(call("ndc_ndvi_month", list(roi = poly, year = as.integer(format(from, "%Y")), month = as.integer(format(from, "%m")))))
    } else {
      list(call("ndc_ndvi_stack", list(roi = poly, trange = c(from, to))))
    },

    "Weather" = c(
      list(call("adc_weather_station", list(roi = row$wkt), slot = "station")),
      if (!is.null(station)) {
        if (!is.na(from) && !is.na(to) && from == to) {
          list(call("adc_weather_date", list(station = station, date = from)))
        } else {
          list(call("adc_weather_long_period", list(station = station, trange = c(from, to), by_days = weather_chunk_days,
                                                    sleep_sec = weather_chunk_sleep)))
        }
      }
    ),

    list()
  )
}

# The files of the rows of the overview `ov` (without ".json"), named as in the download: the dataset, the view and the polygon, and when
# that is taken by an earlier row the period of the row, or a number
parameter_base_names <- function(ov) {
  taken <- character(0)
  vapply(seq_len(nrow(ov)), function(i) {
    base <- tolower(safe_filename(paste0(ov$dataset[i], "_", ov$view[i], "_", ov$polygon[i])))
    period <- tolower(safe_filename(gsub(" - ", "_to_", export_date_label(ov$year[i], ov$date_from[i], ov$date_to[i]))))
    candidates <- c(base, if (nzchar(period)) paste0(base, "_", period), paste0(base, "_", 2:999))
    name <- candidates[!candidates %in% taken][1]
    taken <<- c(taken, name)
    name
  }, "")
}

# How many files will the overview have? Weather has two (the station and the data), the others one per row (none for the NDVI
# statistics of an area that is no project area). Does not ask anything of the services.
parameter_file_count <- function(ov) {
  sum(vapply(seq_len(nrow(ov)), function(i) {
    row <- ov[i, , drop = FALSE]
    if (identical(as.character(row$dataset), "Weather")) 2L else length(row_calls(row))
  }, 1L))
}

# The id of the weather station that Weather retrieves for the row (NULL if it cannot be found), as the retrieval asks for it
weather_station_id <- function(row, adc_token) {
  id <- tryCatch(rNDC::adc_weather_station(row$wkt, token = adc_token)$closest_id, error = function(e) NULL)
  if (length(id) != 1 || is.na(id) || !nzchar(id)) NULL else id
}

# Write the parameter files of the overview `ov` to the folder `dir`. Returns list(files, notes): a table of what was written (the file,
# the dataset, the polygon, the rNDC function) and the sentences about the rows that have no file, with the reason.
parameters_export <- function(ov, dir, adc_token = "", progress = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  bases <- parameter_base_names(ov)
  files <- list()
  notes <- character(0)

  for (i in seq_len(nrow(ov))) {
    row <- ov[i, , drop = FALSE]
    label <- paste0(row$dataset, ", ", row$view, ", ", row$polygon)
    if (!is.null(progress)) progress(i, nrow(ov))

    station <- NULL
    if (identical(as.character(row$dataset), "Weather")) {
      station <- weather_station_id(row, adc_token)
      if (is.null(station)) notes <- c(notes, paste0(label, ": the weather station could not be found, so there is only the file of the station."))
    }
    calls <- row_calls(row, station)
    if (length(calls) == 0) {
      notes <- c(notes, paste0(label, ": no file (", if (identical(as.character(row$dataset), "NDVI")) "NDVI statistics exist for the LTER and SNL project areas only"
                               else "this dataset is not retrieved yet", ")."))
    }
    for (cl in calls) {
      file <- paste0(bases[i], if (identical(cl$slot, "station")) "_station", ".json")
      rNDC::ndc_params_write(cl$fun, cl$args, file.path(dir, file))
      files[[length(files) + 1]] <- tibble::tibble(file = file, dataset = row$dataset, view = row$view, polygon = row$polygon, fun = cl$fun)
    }
  }
  list(files = if (length(files) > 0) dplyr::bind_rows(files) else tibble::tibble(file = character(), dataset = character(),
                                                                                  view = character(), polygon = character(),
                                                                                  fun = character()),
       notes = notes)
}

# The text that goes with the files in a zip: how to run them
parameters_readme <- function(export) {
  c("Parameters of the datasets of the overview of the NatureDataCube app (rNDC.Shiny), saved without retrieving them.",
    "",
    "Each .json file is the record of one call of an rNDC function: the area, the year or the period and the other arguments. It never",
    "has a token. Run a file in R with rNDC::ndc_repeat(), giving the token (the AgroDataCube one for Weather and Agricultural fields):",
    "",
    "  library(rNDC)",
    paste0("  ndc_repeat(\"", if (nrow(export$files) > 0) export$files$file[1] else "file.json", "\", token = Sys.getenv(\"NDC_TOKEN\"))"),
    "",
    "Weather has two files: *_station.json finds the closest weather station of the area, and the other has the data of that station.",
    if (nrow(export$files) > 0) c("", "Files:", paste0("  ", export$files$file, "  <- ", export$files$fun, "()  ", export$files$dataset, ", ",
                                                       export$files$view, ", ", export$files$polygon)),
    if (length(export$notes) > 0) c("", "Not written:", paste0("  ", export$notes)))
}
