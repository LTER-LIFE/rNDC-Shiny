# Which datasets have data for the selected area? One row per dataset: a `status` and a sentence (`detail`) that
# says it. The check is quick (a few requests, 0.05-0.3 s for AgroDataCube, 0.5-2 s for the STAC search) and its
# parts are cached per area. The app makes it when the "Check availability" button is clicked (all datasets) and when
# a dataset is added to the overview (that dataset, for each polygon): a dataset with no data for a polygon is not added.
# It tells what the services know about the area.
#
# Statuses: "yes" (data found), "partial" (some of it, e.g. not all nitrogen layers), "no" (none found),
# "unknown" (the check failed or could not be made) and "na" (does not apply: not wired yet, needs a token that is
# missing, or only for project areas).

availability_statuses <- c("yes", "partial", "no", "unknown", "na")

# The default for how far (km) the weather station may be from the centre of the area; the user can change it
weather_max_station_km <- 30

availability_row <- function(dataset, status, detail = "") {
  data.frame(dataset = dataset, status = status, detail = detail, stringsAsFactors = FALSE)
}

# The reason that the last attempt for a key failed, to say it in the detail of the row and in the console
availability_errors <- new.env(parent = emptyenv())

# Compute `compute()` once per `key`: cached for all sessions. A request that fails is tried once more (the services
# fail now and then); if it fails again the result is NULL (not cached), and the reason is printed and kept for
# `availability_could_not()`.
availability_cached <- function(key, compute) {
  value <- cache_get(key)
  if (!is.null(value)) return(value)
  attempt <- function() tryCatch(compute(), error = function(e) e)
  value <- attempt()
  if (inherits(value, "error")) value <- attempt()
  if (inherits(value, "error")) {
    reason <- conditionMessage(value)
    assign(key, reason, envir = availability_errors)
    message("Availability check failed (", sub("^avail\\|([^|]*)\\|.*$", "\\1", key), "): ", reason)
    return(NULL)
  }
  rm(list = intersect(key, ls(availability_errors)), envir = availability_errors)
  cache_set(key, value)
}

# "Could not check for <what>." with the reason it failed, if one is known for `key`
availability_could_not <- function(what, key) {
  reason <- availability_errors[[key]]
  paste0("Could not check for ", what, if (!is.null(reason)) paste0(" (", substr(reason, 1, 200), ")") else "", ".")
}

# The outline of the Netherlands, in the Dutch grid (bundled with rNDC), read once per process; NULL if it cannot be read
availability_nl_outline <- local({
  outline <- NULL
  function() {
    if (is.null(outline)) outline <<- tryCatch(rNDC::ndc_netherlands(), error = function(e) NULL)
    outline
  }
})

# ---- the checks ----

# AgroDataCube: does the request for `option` return a feature for the area? A request with the polygon
# `geometry` (an sfc) as `adc_geometry()` shortens it for the URL. NULL if the request fails.
availability_adc_features <- function(option, geometry, extra, token, test = function(features) length(features) > 0) {
  wkt <- sf::st_as_text(geometry)
  row <- tibble::tibble(wkt = wkt, polygon_sf = list(sf::st_sf(geometry = geometry)))
  wkt <- adc_geometry(row)$wkt
  response <- if (option == "AHN") {
    rNDC::adc_get(url = rNDC::adc_url("AHN", params = c(geometry = wkt, epsg = "4326")), token = token)
  } else {
    rNDC::adc_get(option = option, params = c(geometry = wkt, epsg = "4326", output_epsg = "4326", extra, page_size = "1"),
                  token = token)
  }
  test(response$features)
}

# a status from a TRUE/FALSE, or NA/NULL (the request failed)
availability_status_of <- function(found) if (is.null(found) || is.na(found)) "unknown" else if (isTRUE(found)) "yes" else "no"

availability_weather <- function(geometry, key, token, max_km = weather_max_station_km) {
  cache <- cache_key("avail", "weather", key)
  distance <- availability_cached(cache, function() {
    min(rNDC::adc_weather_station(sf::st_as_text(geometry), token = token)$distances) / 1000
  })
  if (is.null(distance)) return(list(status = "unknown", detail = availability_could_not("a weather station", cache)))
  km <- round(distance)
  if (distance <= max_km) {
    list(status = "yes", detail = paste0("A weather station is ", km, " km from the selected area."))
  } else {
    list(status = "no", detail = paste0("The closest weather station is ", km, " km from the selected area (the limit is ",
                                        format(max_km), " km)."))
  }
}

availability_fields <- function(geometry, key, token, year) {
  if (is.null(year) || is.na(suppressWarnings(as.integer(year)))) {
    return(list(status = "unknown", detail = "Choose a year to check for Agricultural fields."))
  }
  cache <- cache_key("avail", "fields", key, year)
  found <- availability_cached(cache, function() availability_adc_features("Fields", geometry, c(year = as.character(year)), token))
  status <- availability_status_of(found)
  list(status = status, detail = switch(status,
    yes = paste0("Agricultural fields for ", year, " are available for the selected area."),
    no = paste0("No Agricultural fields for ", year, " found for the selected area."),
    availability_could_not("Agricultural fields", cache)))
}

# Land Use, Nitrogen, Soil map and AHN: from the table of `stac_availability()`
availability_raster <- function(dataset, collections, table, year) {
  if (is.null(table)) return(list(status = "unknown", detail = paste0("Could not check for ", dataset, " data.")))
  has_year <- !is.null(year) && nzchar(as.character(year))
  what <- if (has_year) paste0(dataset, " data for ", year) else paste0(dataset, " data")
  present <- vapply(collections, function(collection) {
    rows <- table[table$collection == collection, , drop = FALSE]
    if (nrow(rows) == 0) return(FALSE)
    if (anyNA(rows$n_items)) return(NA)
    if (!has_year) return(any(rows$n_items > 0))
    # a row without a year comes from a separate question about the collection: it cannot say which years
    any(rows$n_items[is.na(rows$year) | rows$year == as.character(year)] > 0)
  }, NA)
  missing <- collections[!is.na(present) & !present]
  if (anyNA(present)) {
    list(status = "unknown", detail = paste0("Could not check for ", dataset, " data."))
  } else if (length(missing) == 0) {
    list(status = "yes", detail = paste0(what, " is available for the selected area."))
  } else {
    layers <- if (length(collections) > 1) paste0(" (", paste(missing, collapse = ", "), ")") else ""
    list(status = if (length(missing) == length(collections)) "no" else "partial",
         detail = paste0("No ", what, " found for the selected area", layers, "."))
  }
}

# Does the area (an sfc in EPSG:4326) intersect the Netherlands? TRUE, FALSE, or NA if that cannot be told. The items of the
# country-wide rasters (AHN, soil map, land use) are rectangles that include part of the North Sea, so a search of the STAC
# API alone finds them for a polygon at sea.
availability_in_netherlands <- function(geometry) {
  outline <- availability_nl_outline()
  if (is.null(outline)) return(NA)
  tryCatch(any(sf::st_intersects(sf::st_transform(geometry, sf::st_crs(outline)), outline, sparse = FALSE)), error = function(e) NA)
}

# The check of a dataset whose rasters cover the Netherlands only: no data where the area is outside it
availability_only_in_netherlands <- function(result, dataset, geometry) {
  if (result$status %in% c("yes", "partial") && identical(availability_in_netherlands(geometry), FALSE)) {
    return(list(status = "no", detail = paste0("No ", dataset, " data found for the selected area: it is outside the Netherlands.")))
  }
  result
}

# NDVI: the rasters (the `ndvi` collection of the NatureDataCube, from GroenMonitor) cover the Netherlands; the statistics exist for the LTER and SNL project areas.
# With the `view` ("Geodata" or "Statistics") only the one that is asked for counts; without it either will do.
availability_ndvi <- function(geometry, area, view = NULL) {
  inside <- availability_in_netherlands(geometry)
  rasters <- list(status = if (is.na(inside)) "unknown" else if (inside) "yes" else "no", detail = "")
  sources <- if ("source_name" %in% names(area)) as.character(area$source_name) else character(nrow(area))
  n_project <- sum(!is.na(vapply(sources, project_ndvi_collection, "")))
  stats_text <- if (n_project > 0) {
    paste0("statistics for ", n_project, " of ", nrow(area), " selected polygon(s) (project areas)")
  } else {
    "no statistics (only for LTER and SNL project areas)"
  }
  raster_text <- switch(rasters$status,
    yes = "rasters for the selected area",
    no = "no rasters (the selected area is outside the Netherlands)",
    "rasters not checked")
  view <- tolower(as.character(view))
  if (identical(view, "statistics")) {
    return(list(status = if (n_project > 0) "yes" else "no", detail = paste0("NDVI: ", stats_text, ".")))
  }
  if (identical(view, "geodata")) {
    return(list(status = rasters$status, detail = paste0("NDVI: ", raster_text, ".")))
  }
  status <- if (rasters$status == "yes" || n_project > 0) "yes" else if (rasters$status == "no") "no" else "unknown"
  list(status = status, detail = paste0("NDVI: ", raster_text, "; ", stats_text, "."))
}

# The datasets that cannot be checked, with the reason
availability_unwired <- function(dataset) list(status = "na", detail = paste0(dataset, " is not available yet."))

# The check for the area. `area` is the sf object of the selected polygons (with a `source_name`), `years` a
# list with the year of "Agricultural fields", "Land Use" and "Nitrogen" (a missing year means any), and
# `weather_max_km` the largest distance to the weather station that counts as data. `datasets` are the datasets to
# check (all by default; only their requests are made) and `view` the view of NDVI ("Geodata" or "Statistics"; either
# by default). Returns a data frame with a row for each dataset, in the order of `all_dataset_names`.
availability_check <- function(area, ndc_token, adc_token, years = list(), weather_max_km = weather_max_station_km,
                               datasets = all_dataset_names, view = NULL) {
  datasets <- intersect(all_dataset_names, datasets)
  geometry <- sf::st_union(sf::st_geometry(area))
  key <- sf::st_as_text(geometry, digits = 6)
  adc_ok <- !is.null(adc_token) && nzchar(adc_token)
  needs_adc <- list(status = "na", detail = "This dataset needs an AgroDataCube token (ADC_TOKEN).")

  # one search tells about all the datasets of the NatureDataCube with rasters
  stac_datasets <- c("Nitrogen", "Soil map", "AHN", "Land Use")
  stac <- if (any(datasets %in% stac_datasets)) {
    availability_cached(cache_key("avail", "stac", key),
                        function() rNDC::stac_availability(c(ndc_landuse_collection, nitrogen_layer_choices, ndc_soil_collection,
                                                             ndc_ahn_collection), geometry, ndc_token))
  }

  check <- list(
    "Weather" = function() if (adc_ok) availability_weather(geometry, key, adc_token, weather_max_km) else needs_adc,
    "Nitrogen" = function() availability_raster("Nitrogen", nitrogen_layer_choices, stac, years[["Nitrogen"]]),
    "NDVI" = function() availability_ndvi(geometry, area, view),
    "Vegetation structure" = function() availability_unwired("Vegetation structure"),
    "Ground water table" = function() availability_unwired("Ground water table"),
    "Soil map" = function() availability_only_in_netherlands(availability_raster("Soil map", ndc_soil_collection, stac, NULL),
                                                             "Soil map", geometry),
    "AHN" = function() availability_only_in_netherlands(availability_raster("AHN", ndc_ahn_collection, stac, NULL), "AHN", geometry),
    "Agricultural fields" = function() if (adc_ok) availability_fields(geometry, key, adc_token, years[["Agricultural fields"]]) else needs_adc,
    "Land Use" = function() availability_only_in_netherlands(availability_raster("Land Use", ndc_landuse_collection, stac, years[["Land Use"]]),
                                                             "Land Use", geometry)
  )
  rows <- lapply(datasets, function(dataset) {
    r <- check[[dataset]]()
    availability_row(dataset, r$status, r$detail)
  })
  if (length(rows) == 0) return(availability_row(character(), character(), character()))
  do.call(rbind, rows)
}

# The rows of `new` replace those of the same datasets in `old` (the table of an earlier check, or NULL), in the order of
# `all_dataset_names`
availability_merge <- function(old, new) {
  if (is.null(old)) return(new[order(match(new$dataset, all_dataset_names)), , drop = FALSE])
  out <- rbind(old[!old$dataset %in% new$dataset, , drop = FALSE], new)
  out <- out[order(match(out$dataset, all_dataset_names)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# One row for a dataset from the rows of its checks for several polygons (a list of one-row data frames, NULL where the
# check could not be made): the status they share, or "partial" where some have data and some do not
availability_combine <- function(rows) {
  dataset <- rows[[which(!vapply(rows, is.null, NA))[1]]]$dataset
  status <- vapply(rows, function(r) if (is.null(r)) "unknown" else r$status, "")
  if (length(unique(status)) == 1) {
    first <- rows[[1]]
    return(availability_row(dataset, status[1], if (is.null(first)) "Could not check for the selected polygons." else first$detail))
  }
  found <- status %in% c("yes", "partial")
  if (any(found)) {
    return(availability_row(dataset, "partial", paste0(dataset, " data found for ", sum(found), " of ", length(rows),
                                                         " selected polygons.")))
  }
  availability_row(dataset, "unknown", paste0("Could not check ", dataset, " for all the selected polygons."))
}

# The row of `table` (from `availability_check()`) for `dataset`, or NULL
availability_of <- function(table, dataset) {
  if (is.null(table) || is.null(dataset)) return(NULL)
  row <- table[table$dataset == dataset, , drop = FALSE]
  if (nrow(row) == 0) NULL else row
}

# The message for a dataset that is not added where no data was found: `row` is its check and `polygons` the names of the
# polygons without data
availability_no_data_message <- function(row, polygons = character()) {
  where <- if (length(polygons) > 0) paste0(" (", paste(polygons, collapse = ", "), ")") else ""
  paste0(sub("\\.$", "", row$detail), where, ". Nothing was added: change the area, the year or the period, and try again.")
}
