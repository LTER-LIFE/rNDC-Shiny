# Which datasets have data for the selected area? One row per dataset: a `status` and a sentence (`detail`) that
# says it. The check is quick (a few requests, 0.05-0.3 s for AgroDataCube, 0.5-2 s for the STAC search) and its
# parts are cached per area, so it can be done whenever the selection changes. It tells what the services know about
# the area; a dataset can have data even where it says "no", so the app warns but does not refuse.
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

# Compute `compute()` once per `key`: cached for all sessions. A NULL result (a failed request) is not cached.
availability_cached <- function(key, compute) {
  value <- cache_get(key)
  if (is.null(value)) value <- cache_set(key, tryCatch(compute(), error = function(e) NULL))
  value
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
  distance <- availability_cached(cache_key("avail", "weather", key), function() {
    min(rNDC::adc_weather_station(sf::st_as_text(geometry), token = token)$distances) / 1000
  })
  if (is.null(distance)) return(list(status = "unknown", detail = "Could not check for a weather station."))
  km <- round(distance)
  if (distance <= max_km) {
    list(status = "yes", detail = paste0("A weather station is ", km, " km from the selected area."))
  } else {
    list(status = "no", detail = paste0("The closest weather station is ", km, " km from the selected area (the limit is ",
                                        format(max_km), " km)."))
  }
}

availability_soil <- function(geometry, key, token) {
  found <- availability_cached(cache_key("avail", "soil", key),
                        function() availability_adc_features("Soiltypes", geometry, NULL, token))
  status <- availability_status_of(found)
  list(status = status, detail = switch(status,
    yes = "Soil map data is available for the selected area.",
    no = "No Soil map data found for the selected area.",
    "Could not check for Soil map data."))
}

availability_ahn <- function(geometry, key, token) {
  # AHN answers with one feature for any area; without elevation data its properties are empty
  has_values <- function(features) length(features) > 0 && length(Filter(Negate(is.null), features[[1]]$properties)) > 0
  found <- availability_cached(cache_key("avail", "ahn", key),
                        function() availability_adc_features("AHN", geometry, NULL, token, test = has_values))
  status <- availability_status_of(found)
  list(status = status, detail = switch(status,
    yes = "Elevation (AHN) data is available for the selected area.",
    no = "No elevation (AHN) data found for the selected area.",
    "Could not check for elevation (AHN) data."))
}

availability_fields <- function(geometry, key, token, year) {
  if (is.null(year) || is.na(suppressWarnings(as.integer(year)))) {
    return(list(status = "unknown", detail = "Choose a year to check for Agricultural fields."))
  }
  found <- availability_cached(cache_key("avail", "fields", key, year),
                        function() availability_adc_features("Fields", geometry, c(year = as.character(year)), token))
  status <- availability_status_of(found)
  list(status = status, detail = switch(status,
    yes = paste0("Agricultural fields for ", year, " are available for the selected area."),
    no = paste0("No Agricultural fields for ", year, " found for the selected area."),
    "Could not check for Agricultural fields."))
}

# Land Use and Nitrogen: from the table of `stac_availability()`
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

# NDVI: the rasters (GroenMonitor) cover the Netherlands; the statistics exist for the LTER and SNL project areas
availability_ndvi <- function(geometry, area) {
  outline <- availability_nl_outline()
  rasters <- if (is.null(outline)) {
    list(status = "unknown", detail = "")
  } else {
    inside <- tryCatch(any(sf::st_intersects(sf::st_transform(geometry, sf::st_crs(outline)), outline, sparse = FALSE)),
                       error = function(e) NA)
    if (is.na(inside)) list(status = "unknown", detail = "")
    else list(status = if (inside) "yes" else "no", detail = "")
  }
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
  status <- if (rasters$status == "yes" || n_project > 0) "yes" else if (rasters$status == "no") "no" else "unknown"
  list(status = status, detail = paste0("NDVI: ", raster_text, "; ", stats_text, "."))
}

# The datasets that cannot be checked, with the reason
availability_unwired <- function(dataset) list(status = "na", detail = paste0(dataset, " is not available yet."))

# The check for the whole area. `area` is the sf object of the selected polygons (with a `source_name`), `years` a
# list with the year of "Agricultural fields", "Land Use" and "Nitrogen" (a missing year means any), and
# `weather_max_km` the largest distance to the weather station that counts as data. Returns a data frame
# with a row for each dataset, in the order of `all_dataset_names`.
availability_check <- function(area, ndc_token, adc_token, years = list(), weather_max_km = weather_max_station_km) {
  geometry <- sf::st_union(sf::st_geometry(area))
  key <- sf::st_as_text(geometry, digits = 6)
  adc_ok <- !is.null(adc_token) && nzchar(adc_token)
  needs_adc <- list(status = "na", detail = "This dataset needs an AgroDataCube token (ADC_TOKEN).")

  stac <- availability_cached(cache_key("avail", "stac", key),
                       function() rNDC::stac_availability(c(ndc_landuse_collection, nitrogen_layer_choices), geometry, ndc_token))

  results <- list(
    "Weather" = if (adc_ok) availability_weather(geometry, key, adc_token, weather_max_km) else needs_adc,
    "Nitrogen" = availability_raster("Nitrogen", nitrogen_layer_choices, stac, years[["Nitrogen"]]),
    "NDVI" = availability_ndvi(geometry, area),
    "Vegetation structure" = availability_unwired("Vegetation structure"),
    "Ground water table" = availability_unwired("Ground water table"),
    "Soil map" = if (adc_ok) availability_soil(geometry, key, adc_token) else needs_adc,
    "AHN" = if (adc_ok) availability_ahn(geometry, key, adc_token) else needs_adc,
    "Agricultural fields" = if (adc_ok) availability_fields(geometry, key, adc_token, years[["Agricultural fields"]]) else needs_adc,
    "Land Use" = availability_raster("Land Use", ndc_landuse_collection, stac, years[["Land Use"]])
  )
  rows <- lapply(all_dataset_names, function(dataset) {
    r <- results[[dataset]]
    availability_row(dataset, r$status, r$detail)
  })
  do.call(rbind, rows)
}

# The row of `table` (from `availability_check()`) for `dataset`, or NULL
availability_of <- function(table, dataset) {
  if (is.null(table) || is.null(dataset)) return(NULL)
  row <- table[table$dataset == dataset, , drop = FALSE]
  if (nrow(row) == 0) NULL else row
}

# The message for a dataset that is added where no data was found
availability_no_data_warning <- function(row) {
  paste0(sub("\\.$", "", row$detail), ". It is added anyway: the services may still have data for it.")
}
