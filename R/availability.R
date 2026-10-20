# Which datasets have data for the selected area? One row per dataset: a `status` and a sentence (`detail`) that
# says it. The check is quick (a few requests, 0.05-0.3 s for AgroDataCube, 0.5-2 s for the STAC search) and its
# parts are cached per area. The app makes it when the "Check data availability" button is clicked (all datasets) and when
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

availability_weather <- function(geometry, key, token, max_km = weather_max_station_km, period = NULL) {
  cache <- cache_key("avail", "weather", key)
  station <- availability_cached(cache, function() {
    closest <- rNDC::adc_weather_station(sf::st_as_text(geometry), token = token)
    list(id = closest$closest_id, km = min(closest$distances) / 1000)
  })
  if (is.null(station)) return(list(status = "unknown", detail = availability_could_not("a weather station", cache)))
  km <- round(station$km)
  if (station$km > max_km) {
    return(list(status = "no", detail = paste0("The closest weather station is ", km, " km from the selected area (the limit is ",
                                               format(max_km), " km).")))
  }
  if (is.null(period)) return(list(status = "yes", detail = paste0("A weather station is ", km, " km from the selected area.")))
  availability_weather_observations(station, km, period, token)
}

# The days with observations of the station in the period (up to one page of 500 days; more is "at least"); if there are none,
# when its observations start
availability_weather_observations <- function(station, km, period, token) {
  from <- format(as.Date(period[1]), "%Y%m%d")
  to <- format(as.Date(period[2]), "%Y%m%d")
  observed <- function(params) {
    res <- rNDC::adc_get(option = "Meteo_data", params = c(output_epsg = "4326", meteostation = as.character(station$id), params,
                                                           page_offset = "0"), token = token)
    unique(stats::na.omit(vapply(res$features, function(f) as.character(f$properties$datum %||% NA_character_), "")))
  }
  cache <- cache_key("avail", "weather-days", station$id, from, to)
  days <- availability_cached(cache, function() observed(c(fromdate = from, todate = to, page_size = "500")))
  who <- paste0("station ", station$id, " (", km, " km away)")
  if (is.null(days)) return(list(status = "unknown", detail = availability_could_not("Weather observations", cache)))
  between <- availability_period_text(period)
  if (length(days) > 0) {
    n <- if (length(days) >= 500) "at least 500 days" else paste0(length(days), if (length(days) == 1) " day" else " days")
    return(list(status = "yes", detail = paste0("Weather: ", who, " has observations on ", n, " between ", between, ".")))
  }
  first <- availability_cached(cache_key("avail", "weather-first", station$id),
                               function() observed(c(fromdate = "19000101", todate = format(Sys.Date(), "%Y%m%d"), page_size = "1")))
  since <- if (length(first) > 0) paste0(" Its observations start on ", first[1], ".") else ""
  list(status = "no", detail = paste0("Weather: ", who, " has no observations between ", between, ".", since))
}

availability_fields <- function(geometry, key, token, year) {
  if (is.null(year) || is.na(suppressWarnings(as.integer(year)))) {
    return(list(status = "unknown", detail = "Choose a year to check for Agricultural fields."))
  }
  cache <- cache_key("avail", "fields", key, year)
  found <- availability_cached(cache, function() availability_adc_features("Fields", geometry, c(year = as.character(year)), token))
  status <- availability_status_of(found)
  last_year <- as.integer(format(Sys.Date(), "%Y")) - 1L
  list(status = status, detail = switch(status,
    yes = paste0("Agricultural fields ", year, ": fields found in the selected area."),
    no = paste0("No agricultural fields in ", year, " for the selected area (the register has fields from ", fields_min_year,
                " to ", last_year, ")."),
    availability_could_not("Agricultural fields", cache)))
}

# "a", "a and b", "a, b and c"
availability_and <- function(x) if (length(x) <= 1) x else paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])

# Land Use, Nitrogen, Soil map and AHN: from the table of `stac_availability()`. With a year the message is about that year, and when
# it has no data the years that have are listed.
availability_raster <- function(dataset, collections, table, year) {
  if (is.null(table)) return(list(status = "unknown", detail = paste0("Could not check for ", dataset, " data.")))
  has_year <- !is.null(year) && nzchar(as.character(year))
  present <- vapply(collections, function(collection) {
    rows <- table[table$collection == collection, , drop = FALSE]
    if (nrow(rows) == 0) return(FALSE)
    if (anyNA(rows$n_items)) return(NA)
    if (!has_year) return(any(rows$n_items > 0))
    # a row without a year comes from a separate question about the collection: it cannot say which years
    any(rows$n_items[is.na(rows$year) | rows$year == as.character(year)] > 0)
  }, NA)
  years <- sort(unique(table$year[table$collection %in% collections & !is.na(table$year) & !is.na(table$n_items) & table$n_items > 0]))
  missing <- collections[!is.na(present) & !present]
  where <- " for the selected area"
  if (anyNA(present)) {
    return(list(status = "unknown", detail = paste0("Could not check for ", dataset, " data.")))
  }
  label <- if (has_year) paste0(dataset, " ", year) else dataset
  if (length(missing) == 0) {
    shown <- if (length(collections) > 1) paste0(availability_and(collections), " available") else "raster available"
    in_years <- if (!has_year && length(years) > 0) paste0(" (", if (length(years) == 1) "year " else "years ", paste(years, collapse = ", "), ")") else ""
    return(list(status = "yes", detail = paste0(label, ": ", shown, where, in_years, ".")))
  }
  if (length(missing) < length(collections)) {
    return(list(status = "partial", detail = paste0(label, ": only ", availability_and(setdiff(collections, missing)), " available", where,
                                                    " (", availability_and(missing), " missing).")))
  }
  list(status = "no", detail = paste0("No ", dataset, " data", if (has_year) paste0(" for ", year) else "", " found", where, ".",
                                      if (has_year && length(years) > 0) paste0(" Available years: ", paste(years, collapse = ", "), ".") else ""))
}

# AHN: the same, and which layers there are (terrain height, object height) and for which years
availability_ahn <- function(table, geometry, ndc_token) {
  result <- availability_raster("AHN", ndc_ahn_collection, table, NULL)
  if (!result$status %in% c("yes", "partial")) return(result)
  key <- sf::st_as_text(geometry, digits = 6)
  layers <- availability_cached(cache_key("avail", "ahn-layers", key), function() {
    items <- rNDC::stac_collect_metadata(geometry, ndc_token, rNDC::ndc_endpoint(), ndc_ahn_collection)
    unique(items[!is.na(items$layer), c("layer", "year")])
  })
  if (is.null(layers) || nrow(layers) == 0) return(result)
  layers <- layers[order(layers$layer), , drop = FALSE]
  named <- vapply(seq_len(nrow(layers)), function(i) {
    label <- ahn_layer_labels[[tolower(layers$layer[i])]] %||% layers$layer[i]
    paste0(label, " (", layers$year[i], ")")
  }, "")
  list(status = result$status, detail = paste0("AHN: ", availability_and(named), " available for the selected area."))
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

# NDVI: the rasters (the `ndvi` collection of the NatureDataCube, from GroenMonitor) cover the Netherlands; the statistics exist for the
# LTER and SNL project areas. With the `view` ("Geodata" or "Statistics") only the one that is asked for counts; without it either will
# do. With a `period` (two dates) the services are asked what they have in it: the days with rasters, or the observations of the
# project polygons (no data in the period is "no"); without it the coverage is told.
availability_ndvi <- function(geometry, area, view = NULL, period = NULL, ndc_token = Sys.getenv("NDC_TOKEN")) {
  inside <- availability_in_netherlands(geometry)
  sources <- if ("source_name" %in% names(area)) as.character(area$source_name) else character(nrow(area))
  n_project <- sum(!is.na(vapply(sources, project_ndvi_collection, "")))
  view <- tolower(as.character(view))
  rasters <- function() {
    if (is.na(inside)) return(list(status = "unknown", detail = "NDVI: it could not be checked whether the selected area is in the Netherlands."))
    if (!inside) return(list(status = "no", detail = "No NDVI rasters: the selected area is outside the Netherlands."))
    if (!is.null(period)) return(availability_ndvi_rasters(geometry, period, ndc_token))
    list(status = "yes", detail = "NDVI rasters cover the selected area.")
  }
  statistics <- function() {
    if (n_project == 0) return(list(status = "no", detail = "No NDVI statistics: they exist only for LTER and SNL project areas."))
    if (!is.null(period)) return(availability_ndvi_statistics(area, period, ndc_token))
    list(status = "yes", detail = paste0("NDVI statistics for ", n_project, " of ", nrow(area), " selected polygon(s) (project areas)."))
  }
  if (identical(view, "statistics")) return(statistics())
  if (identical(view, "geodata")) return(rasters())

  # either view will do (no view was asked for)
  r <- rasters()
  status <- if (r$status == "yes" || n_project > 0) "yes" else if (r$status == "no") "no" else "unknown"
  raster_text <- switch(r$status, yes = "rasters cover the selected area", no = "no rasters (the selected area is outside the Netherlands)",
                        "rasters not checked")
  stats_text <- if (n_project > 0) {
    paste0("statistics for ", n_project, " of ", nrow(area), " selected polygon(s) (project areas)")
  } else {
    "no statistics (only for LTER and SNL project areas)"
  }
  list(status = status, detail = paste0("NDVI: ", raster_text, "; ", stats_text, ". To check a period, click the Check data availability button next to the Add to overview button."))
}

# "2024-06-01 and 2024-06-30"
availability_period_text <- function(period) paste0(format(as.Date(period[1])), " and ", format(as.Date(period[2])))

# The days with NDVI rasters in the period for the area: the items of the `ndvi` collection (one a day, only the days with data)
availability_ndvi_rasters <- function(geometry, period, ndc_token) {
  key <- sf::st_as_text(geometry, digits = 6)
  days <- availability_cached(cache_key("avail", "ndvi-rasters", key, format(period[1]), format(period[2])), function() {
    as.numeric(rNDC::ndc_count(collection = "ndvi", roi = geometry, trange = as.character(period), limit = 1, token = ndc_token))
  })
  cache <- cache_key("avail", "ndvi-rasters", key, format(period[1]), format(period[2]))
  if (is.null(days) || is.na(days)) return(list(status = "unknown", detail = availability_could_not("NDVI rasters", cache)))
  between <- availability_period_text(period)
  if (days == 0) return(list(status = "no", detail = paste0("No NDVI rasters between ", between, " for the selected area.")))
  list(status = "yes", detail = paste0("NDVI rasters: ", days, if (days == 1) " day" else " days", " with data between ", between,
                                       " for the selected area."))
}

# The observations of the project polygons `area` in the period: the monthly statistics of rNDC::ndc_ndvi_stats()
availability_ndvi_statistics <- function(area, period, ndc_token) {
  sources <- if ("source_name" %in% names(area)) as.character(area$source_name) else character(nrow(area))
  collections <- vapply(sources, project_ndvi_collection, "")
  names <- paste(unique(sources[!is.na(collections)]), collapse = ", ")
  area <- sf::st_transform(area[!is.na(collections), , drop = FALSE], 4326)
  collections <- unique(collections[!is.na(collections)])
  key <- sf::st_as_text(sf::st_union(sf::st_geometry(area)), digits = 6)

  counts <- lapply(collections, function(collection) {
    cache <- cache_key("avail", "ndvi-stats", key, collection, format(period[1]), format(period[2]))
    list(cache = cache, value = availability_cached(cache, function() {
      stats <- rNDC::ndc_ndvi_stats(area, collection, trange = as.character(period), token = ndc_token)
      list(n_obs = sum(stats$n_obs), months = length(unique(stats$month)))
    }))
  })
  failed <- Filter(function(x) is.null(x$value), counts)
  if (length(failed) > 0) return(list(status = "unknown", detail = availability_could_not("NDVI statistics", failed[[1]]$cache)))
  n_obs <- sum(vapply(counts, function(x) x$value$n_obs, 0))
  months <- sum(vapply(counts, function(x) x$value$months, 0))
  between <- availability_period_text(period)
  if (n_obs == 0) return(list(status = "no", detail = paste0("No NDVI statistics between ", between, " for ", names, ".")))
  list(status = "yes", detail = paste0("NDVI statistics: ", n_obs, if (n_obs == 1) " observation" else " observations", " in ", months,
                                       if (months == 1) " month" else " months", " between ", between, " for ", names, "."))
}

# The datasets that cannot be checked, with the reason
availability_unwired <- function(dataset) list(status = "na", detail = paste0(dataset, " is not available yet."))

# The check for the area. `area` is the sf object of the selected polygons (with a `source_name`), `years` a
# list with the year of "Agricultural fields", "Land Use" and "Nitrogen" (a missing year means any), and
# `weather_max_km` the largest distance to the weather station that counts as data. `datasets` are the datasets to
# check (all by default; only their requests are made), `view` the view of NDVI ("Geodata" or "Statistics"; either
# by default) and `period` the first and the last date asked for (two dates; used for NDVI: what the services have in it). `progress`, if given, is a function(done, total) that is called with the number of datasets that have been
# tested, from 0 up to the total. Returns a data frame with a row for each dataset, in the order of `all_dataset_names`.
availability_check <- function(area, ndc_token, adc_token, years = list(), weather_max_km = weather_max_station_km,
                               datasets = all_dataset_names, view = NULL, progress = NULL, period = NULL) {
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
    "Weather" = function() if (adc_ok) availability_weather(geometry, key, adc_token, weather_max_km, period) else needs_adc,
    "Nitrogen" = function() availability_raster("Nitrogen", nitrogen_layer_choices, stac, years[["Nitrogen"]]),
    "NDVI" = function() availability_ndvi(geometry, area, view, period, ndc_token),
    "Vegetation structure" = function() availability_unwired("Vegetation structure"),
    "Ground water table" = function() availability_unwired("Ground water table"),
    "Soil map" = function() availability_only_in_netherlands(availability_raster("Soil map", ndc_soil_collection, stac, NULL),
                                                             "Soil map", geometry),
    "AHN" = function() availability_only_in_netherlands(availability_ahn(stac, geometry, ndc_token), "AHN", geometry),
    "Agricultural fields" = function() if (adc_ok) availability_fields(geometry, key, adc_token, years[["Agricultural fields"]]) else needs_adc,
    "Land Use" = function() availability_only_in_netherlands(availability_raster("Land Use", ndc_landuse_collection, stac, years[["Land Use"]]),
                                                             "Land Use", geometry)
  )
  total <- length(datasets)
  if (!is.null(progress)) progress(0L, total)
  rows <- lapply(seq_along(datasets), function(i) {
    r <- check[[datasets[i]]]()
    if (!is.null(progress)) progress(i, total)
    availability_row(datasets[i], r$status, r$detail)
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

# What the info button next to "Choose dataset(s)" says about the availability of the datasets
availability_help <- function() {
  tagList(
    tags$p("Click ", tags$b("Check data availability"), " (below the map) to find out which datasets have data for the ",
           "selected area. The datasets in the list are marked:"),
    tags$ul(
      tags$li(tags$span(style = "color:#2e7d32; font-weight:700;", "\u2713"), " data found (orange where only part of the data was found, ",
              "for example not all the nitrogen layers)"),
      tags$li(tags$span(style = "color:#b26a00; font-weight:700;", "\u2717"), " no data found"),
      tags$li(tags$span(style = "color:#888; font-weight:700;", "?"), " the check could not be made")
    ),
    tags$p("Point at a dataset, or choose it, for the details. The same click opens a table of all the datasets that exist ",
           "for the area, and for a period if you ask for one."),
    tags$p("Nothing is checked while you select areas or change years and dates. The marks go when you select other polygons, ",
           "or change a year, a date or the distance of the Weather settings, and a warning tells you to check again."),
    tags$p("The button ", tags$b("Check data availability"), " next to ", tags$b("Add to overview"), " does the checks of adding ",
           "the dataset you chose, with the year, date or months you chose, for each selected polygon, without adding it. ",
           "Add to overview is enabled only after this check has passed, and it is disabled again when you change the dataset, ",
           "its view, the year, the date or the months, or the selected polygons. A polygon with no data for the dataset is ",
           "not added, and a message says what to change."),
    contact_note()
  )
}

# The message for a dataset that is not added where no data was found: `row` is its check and `polygons` the names of the
# polygons without data
availability_no_data_message <- function(row, polygons = character(), outcome = "Nothing was added") {
  where <- if (length(polygons) > 0) paste0(" (", paste(polygons, collapse = ", "), ")") else ""
  paste0(sub("\\.$", "", row$detail), where, ". ", outcome, ": change the area, the year or the period, and try again.")
}
