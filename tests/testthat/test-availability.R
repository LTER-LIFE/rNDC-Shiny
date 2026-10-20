# "Which datasets have data for the selected area?": the check per dataset with the services mocked, the weather distance and the
# marks in the app. (R/availability.R, R/server_availability.R)

test_that("raster availability depends on the year, names the missing layers, and lists the years that have data", {
  table <- data.frame(collection = c("ntot", "nox", "ntot"), year = c("2024", "2024", "2025"), n_items = c(1, 2, 1))
  layers <- c("ntot", "nox", "nh3")

  partial <- availability_raster("Nitrogen", layers, table, "2024")
  expect_equal(partial$status, "partial")
  expect_equal(partial$detail, "Nitrogen 2024: only ntot and nox available for the selected area (nh3 missing).")
  all_there <- availability_raster("Nitrogen", c("ntot", "nox"), table, "2024")
  expect_equal(all_there$status, "yes")
  expect_equal(all_there$detail, "Nitrogen 2024: ntot and nox available for the selected area.")
  none <- availability_raster("Nitrogen", layers, table, "2031")
  expect_equal(none$status, "no")
  expect_equal(none$detail, "No Nitrogen data for 2031 found for the selected area. Available years: 2024, 2025.")
  # one raster: the dataset and the year
  one <- availability_raster("Land Use", "ntot", table, "2025")
  expect_equal(one$detail, "Land Use 2025: raster available for the selected area.")
  expect_equal(availability_raster("Land Use", "ntot", table, "1999")$detail,
               "No Land Use data for 1999 found for the selected area. Available years: 2024, 2025.")
  # without a year any year will do, and the years are told
  expect_equal(availability_raster("Nitrogen", "ntot", table, NULL)$detail,
               "Nitrogen: raster available for the selected area (years 2024, 2025).")
  expect_equal(availability_raster("Soil map", "nox", table, NULL)$detail, "Soil map: raster available for the selected area (year 2024).")
  # a collection that was asked separately cannot say which years: it counts for any year
  separate <- data.frame(collection = "lgn", year = NA_character_, n_items = 5)
  expect_equal(availability_raster("Land Use", "lgn", separate, "2024")$status, "yes")
  # nothing found / the check failed
  nothing <- availability_raster("Land Use", "lgn", table[0, ], "2024")
  expect_equal(nothing$status, "no")
  expect_equal(nothing$detail, "No Land Use data for 2024 found for the selected area.")     # (no years to list)
  expect_equal(availability_raster("Land Use", "lgn", NULL, "2024")$status, "unknown")
  failed <- data.frame(collection = "lgn", year = NA_character_, n_items = NA_real_)
  expect_equal(availability_raster("Land Use", "lgn", failed, "2024")$status, "unknown")
})

# the services, answering as they do in the Netherlands
mock_services <- function(env = parent.frame(), fields = 1, distance = 12000,
                          stac = data.frame(collection = c("lgn", "ntot", "nox", "nh3", "soiltypes", "ahnmetrics"),
                                            year = "2024", n_items = 1)) {
  rec <- new.env()
  rec$calls <- character(0)
  rec$meteo <- list()
  # the days (text) with observations of the station for the dates in `params`: all of them by default
  rec$observed <- function(params) {
    from <- as.Date(params[["fromdate"]], "%Y%m%d")
    to <- as.Date(params[["todate"]], "%Y%m%d")
    format(seq(from, min(to, from + 499), by = "day"))
  }
  testthat::local_mocked_bindings(
    adc_get = function(url = NULL, option = NULL, params = NULL, server = "adc", token, ...) {
      if (identical(option, "Meteo_data")) {
        rec$calls <- c(rec$calls, "Meteo_data")
        rec$meteo <- c(rec$meteo, list(params))
        days <- rec$observed(params)
        return(list(features = lapply(days, function(d) list(properties = list(datum = d)))))
      }
      if (!is.null(option)) {
        rec$calls <- c(rec$calls, option)
        rec$params <- params
        return(list(features = if (fields > 0) lapply(seq_len(fields), function(i) list(properties = list(a = 1))) else list()))
      }
      stop("only Fields and Meteo_data are asked of the AgroDataCube")
    },
    adc_weather_station = function(roi, token, ...) {
      rec$calls <- c(rec$calls, "stations")
      list(distances = c(distance + 5000, distance), closest_id = "275")
    },
    .package = "rNDC", .env = env
  )
  testthat::local_mocked_bindings(
    stac_availability = function(collections, roi, token) {
      rec$calls <- c(rec$calls, "stac")
      stac
    },
    stac_collect_metadata = function(...) tibble::tibble(layer = c("dtm", "ohn"), year = c("2019", "2022"), title = c("DTM", "OHN")),
    .package = "rNDC", .env = env
  )
  rec
}

area <- function(name = "Own polygon", x = 5.66, y = 51.99) {
  sf::st_sf(source_name = name, wkt = sf::st_as_text(sf::st_geometry(fixed_square(1, x, y))),
            geometry = sf::st_geometry(fixed_square(1, x, y)))
}

status_of_dataset <- function(table, dataset) table$status[table$dataset == dataset]

test_that("the AHN message names the layers and their years", {
  local_clean_cache()
  table <- data.frame(collection = "ahnmetrics", year = c("2019", "2022"), n_items = 1)
  items <- tibble::tibble(layer = c("ohn", "dtm"), year = c("2022", "2019"), title = c("OHN", "DTM"))
  asked <- new.env()
  asked$n <- 0
  local_mocked_bindings(stac_collect_metadata = function(...) { asked$n <- asked$n + 1; items }, .package = "rNDC")
  geometry <- sf::st_union(sf::st_geometry(area()))
  found <- availability_ahn(table, geometry, "t")
  expect_equal(found$status, "yes")
  expect_equal(found$detail, "AHN: terrain height (2019) and object height (2022) available for the selected area.")
  availability_ahn(table, geometry, "t")
  expect_equal(asked$n, 1)                                            # (cached)

  # the layers could not be read: the general message
  local_clean_cache()
  items <- NULL
  local_mocked_bindings(stac_collect_metadata = function(...) stop("down"), .package = "rNDC")
  expect_equal(availability_ahn(table, geometry, "t")$detail, "AHN: raster available for the selected area (years 2019, 2022).")
  expect_equal(availability_ahn(table[0, ], geometry, "t")$status, "no")
})

test_that("every dataset gets a status for an area in the Netherlands", {
  local_clean_cache()
  mock_services()
  table <- availability_check(area(), "t", "t", years = list("Agricultural fields" = 2023, "Land Use" = "2024", "Nitrogen" = "2024"))

  expect_equal(table$dataset, all_dataset_names)
  expect_equal(unname(setNames(table$status, table$dataset)[c("Weather", "Nitrogen", "Soil map", "AHN", "Agricultural fields", "Land Use")]),
               rep("yes", 6))
  expect_equal(status_of_dataset(table, "NDVI"), "yes")  # the rasters cover the Netherlands
  expect_equal(status_of_dataset(table, "Vegetation structure"), "na")
  expect_equal(status_of_dataset(table, "Ground water table"), "na")
  expect_match(table$detail[table$dataset == "Weather"], "A weather station is 12 km from the selected area.")
  expect_match(table$detail[table$dataset == "Agricultural fields"], "Agricultural fields 2023: fields found in the selected area.", fixed = TRUE)
  expect_equal(table$detail[table$dataset == "Land Use"], "Land Use 2024: raster available for the selected area.")
  expect_equal(table$detail[table$dataset == "AHN"], "AHN: terrain height (2019) and object height (2022) available for the selected area.")
})

test_that("the services say no, and the check says so", {
  local_clean_cache()
  mock_services(fields = 0, distance = 80000, stac = data.frame(collection = character(0), year = character(0), n_items = numeric(0)))
  table <- availability_check(area(), "t", "t", years = list("Agricultural fields" = 2023, "Land Use" = "2024", "Nitrogen" = "2024"))
  for (dataset in c("Weather", "Nitrogen", "Soil map", "AHN", "Agricultural fields", "Land Use")) {
    expect_equal(status_of_dataset(table, dataset), "no", info = dataset)
  }
  expect_match(table$detail[table$dataset == "Weather"], "closest weather station is 80 km from the selected area (the limit is 30 km).", fixed = TRUE)
  expect_match(table$detail[table$dataset == "AHN"], "No AHN data found for the selected area.", fixed = TRUE)
  expect_match(table$detail[table$dataset == "Soil map"], "No Soil map data found for the selected area.", fixed = TRUE)
})

test_that("failed requests are unknown, not no, and are not cached", {
  local_clean_cache()
  rec <- mock_services()
  local_mocked_bindings(adc_get = function(...) stop("down"), adc_weather_station = function(...) stop("down"),
                        .package = "rNDC")
  local_mocked_bindings(stac_availability = function(...) stop("down"), .package = "rNDC")
  table <- availability_check(area(), "t", "t", years = list("Agricultural fields" = 2023))
  for (dataset in c("Weather", "Nitrogen", "Soil map", "AHN", "Agricultural fields", "Land Use")) {
    expect_equal(status_of_dataset(table, dataset), "unknown", info = dataset)
  }
  expect_length(ls(ndc_cache), 0)
})

test_that("a failed check says why, and a request that fails once is tried again", {
  local_clean_cache()
  rm(list = ls(availability_errors), envir = availability_errors)
  calls <- 0
  local_mocked_bindings(adc_get = function(...) { calls <<- calls + 1; stop("HTTP 503") }, .package = "rNDC")
  expect_message(row <- availability_fields(sf::st_geometry(selected_polygon()), "k-fail", "t", 2023), "HTTP 503")
  expect_equal(calls, 2)
  expect_equal(row$status, "unknown")
  expect_match(row$detail, "Could not check for Agricultural fields (HTTP 503", fixed = TRUE)

  calls <- 0
  local_mocked_bindings(adc_get = function(...) {
    calls <<- calls + 1
    if (calls == 1) stop("HTTP 503")
    list(features = list(list(properties = list(a = 1))))
  }, .package = "rNDC")
  expect_equal(availability_fields(sf::st_geometry(selected_polygon()), "k-retry", "t", 2023)$status, "yes")
  expect_equal(calls, 2)
})

test_that("without an AgroDataCube token its datasets do not apply, and Fields wants a year", {
  local_clean_cache()
  rec <- mock_services()
  table <- availability_check(area(), "t", "", years = list())
  for (dataset in c("Weather", "Agricultural fields")) {
    expect_equal(status_of_dataset(table, dataset), "na", info = dataset)
  }
  expect_equal(unname(table$status[table$dataset %in% c("Soil map", "AHN")]), c("yes", "yes"))   # they come from the NatureDataCube
  expect_false(any(c("stations", "Fields") %in% rec$calls))
  expect_equal(status_of_dataset(table, "Land Use"), "yes")

  table <- availability_check(area(), "t", "t", years = list())
  expect_equal(status_of_dataset(table, "Agricultural fields"), "unknown")
  expect_match(table$detail[table$dataset == "Agricultural fields"], "Choose a year")
})

test_that("NDVI: rasters for the Netherlands, statistics for project areas", {
  local_clean_cache()
  mock_services()
  drawn <- availability_check(area("Own polygon_1"), "t", "t")
  expect_equal(status_of_dataset(drawn, "NDVI"), "yes")
  expect_match(drawn$detail[drawn$dataset == "NDVI"], "NDVI: rasters cover the selected area; no statistics (only for LTER and SNL project areas). To check a period, click the Check data availability button next to the Add to overview button.", fixed = TRUE)

  project <- availability_check(area("Nestboxes_3"), "t", "t")
  expect_match(project$detail[project$dataset == "NDVI"], "statistics for 1 of 1 selected polygon")

  abroad <- availability_check(area("Own polygon_1", 4.6, 50.6), "t", "t")  # Belgium
  expect_equal(status_of_dataset(abroad, "NDVI"), "no")
  expect_match(abroad$detail[abroad$dataset == "NDVI"], "outside the Netherlands")

  # a project area outside the outline (it is not asked for) still has statistics
  expect_equal(status_of_dataset(availability_check(area("SNL parcel_2", 4.6, 50.6), "t", "t"), "NDVI"), "yes")
})

test_that("the checks are cached per area, and Fields per year", {
  local_clean_cache()
  rec <- mock_services()
  years <- list("Agricultural fields" = 2023, "Land Use" = "2024", "Nitrogen" = "2024")
  availability_check(area(), "t", "t", years)
  first <- length(rec$calls)
  expect_equal(sum(rec$calls == "stac"), 1)  # one search for Land Use and Nitrogen

  availability_check(area(), "t", "t", years)
  expect_length(rec$calls, first)  # everything from the cache

  years[["Land Use"]] <- "2025"
  availability_check(area(), "t", "t", years)
  expect_length(rec$calls, first)  # the year of a raster needs no new request

  years[["Agricultural fields"]] <- 2022
  availability_check(area(), "t", "t", years)
  expect_length(rec$calls, first + 1)  # only Fields
  expect_equal(rec$calls[length(rec$calls)], "Fields")

  availability_check(area(x = 5.9), "t", "t", years)  # another area
  expect_gt(length(rec$calls), first + 1)
})

test_that("a long polygon is shortened for AgroDataCube like in a retrieval", {
  local_clean_cache()
  rec <- mock_services()
  a <- seq(0, 2 * pi, length.out = 801)
  r <- 0.003 * (1 + 0.05 * sin(7 * a))
  geom <- sf::st_sfc(sf::st_polygon(list(cbind(5.66 + r * cos(a), 51.99 + r * sin(a)))), crs = 4326)
  long <- sf::st_sf(source_name = "Own polygon_1", geometry = geom)
  availability_check(long, "t", "t", years = list("Agricultural fields" = 2023))
  expect_lte(nchar(rec$params[["geometry"]]), adc_max_wkt_chars)
})

test_that("only the datasets that are asked for are checked, and only their requests are made", {
  local_clean_cache()
  rec <- mock_services()
  weather <- availability_check(area(), "t", "t", datasets = "Weather")
  expect_equal(weather$dataset, "Weather")
  expect_equal(rec$calls, "stations")                       # no search of the STAC API

  rec$calls <- character(0)
  rasters <- availability_check(area(), "t", "t", datasets = c("Land Use", "AHN"))
  expect_equal(rasters$dataset, c("AHN", "Land Use"))       # in the order of the menu
  expect_equal(rec$calls, "stac")                            # one search for both

  rec$calls <- character(0)
  expect_equal(availability_check(area(), "t", "t", datasets = "NDVI")$dataset, "NDVI")
  expect_length(rec$calls, 0)                                # the outline of the country needs no request
})

test_that("the rasters of the Netherlands are not found for an area outside it, although their items cover part of the sea", {
  local_clean_cache()
  mock_services()                                   # the search finds the items wherever the area is
  at_sea <- area("Sea", x = 4.2, y = 53.3)
  table <- availability_check(at_sea, "t", "t", years = list("Land Use" = "2024"), datasets = c("AHN", "Soil map", "Land Use", "Nitrogen"))
  expect_equal(unname(setNames(table$status, table$dataset)[c("Soil map", "AHN", "Land Use")]), rep("no", 3))
  expect_match(table$detail[table$dataset == "AHN"], "No AHN data found for the selected area: it is outside the Netherlands.", fixed = TRUE)
  expect_equal(status_of_dataset(table, "Nitrogen"), "yes")             # (the nitrogen rasters are not limited to the land)
  expect_equal(status_of_dataset(availability_check(area(), "t", "t", datasets = "AHN"), "AHN"), "yes")
})

test_that("NDVI statistics exist for project areas only, and the rasters for the Netherlands", {
  local_clean_cache()
  mock_services()
  own <- area("Own polygon_1")
  project <- area("Nestboxes_3")
  geometry <- function(a) sf::st_union(sf::st_geometry(a))
  expect_equal(availability_ndvi(geometry(own), own, "Geodata")$status, "yes")
  expect_equal(availability_ndvi(geometry(own), own, "Statistics")$status, "no")
  expect_match(availability_ndvi(geometry(own), own, "Statistics")$detail, "only for LTER and SNL project areas")
  expect_equal(availability_ndvi(geometry(project), project, "Statistics")$status, "yes")
  expect_equal(availability_ndvi(geometry(own), own)$status, "yes")                    # either view will do
  abroad <- area("Own polygon_1", x = 4.6, y = 50.6)
  expect_equal(availability_ndvi(geometry(abroad), abroad, "Geodata")$status, "no")
})

test_that("the checks of datasets are merged, and those of several polygons combined", {
  old <- data.frame(dataset = c("Weather", "AHN"), status = c("yes", "no"), detail = c("w", "a"), area = "A")
  new <- data.frame(dataset = c("Land Use", "AHN"), status = c("yes", "yes"), detail = c("l", "a2"), area = "B")
  merged <- availability_merge(old, new)
  expect_equal(merged$dataset, c("Weather", "AHN", "Land Use"))     # in the order of the menu
  expect_equal(merged$detail, c("w", "a2", "l"))                    # the new check replaces the old
  expect_equal(availability_merge(NULL, new)$dataset, c("AHN", "Land Use"))

  row <- function(status, detail = status) availability_row("AHN", status, detail)
  expect_equal(availability_combine(list(row("yes", "ok"), row("yes", "ok")))$status, "yes")
  expect_equal(availability_combine(list(row("no", "none"), row("no", "none")))$detail, "none")
  mixed <- availability_combine(list(row("yes"), row("no"), row("no")))
  expect_equal(mixed$status, "partial")
  expect_equal(mixed$detail, "AHN data found for 1 of 3 selected polygons.")
  expect_equal(availability_combine(list(row("no"), NULL))$status, "unknown")
  expect_equal(availability_combine(list(NULL, NULL, row("yes")))$status, "partial")   # data for the one that was checked
  expect_equal(availability_combine(list(NULL, row("no")))$status, "unknown")

  expect_equal(availability_no_data_message(availability_row("AHN", "no", "No AHN data found for the selected area.")),
               "No AHN data found for the selected area. Nothing was added: change the area, the year or the period, and try again.")
  expect_match(availability_no_data_message(availability_row("AHN", "no", "No AHN data found."), c("A", "B")),
               "No AHN data found (A, B). Nothing was added", fixed = TRUE)
})

test_that("nothing is checked until a button is clicked, and the marks go with another area, with a warning", {
  local_clean_cache()
  rec <- mock_check(list("yes", "no"))
  notes <- mock_notes()
  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024", nitrogen_year = "2025", selected_year = 2023)
    selected_polygons(selected_polygon())
    session$flushReact()
    session$elapse(2000)
    expect_length(rec$calls, 0)                           # selecting an area (or a year) checks nothing
    expect_null(availability())
    expect_null(output$availability$html)
    expect_length(notes$notes, 0)                         # no marks, nothing to warn about

    session$setInputs(check_availability = 1)
    expect_length(rec$calls, 1)
    expect_equal(rec$calls[[1]]$datasets, all_dataset_names)
    expect_equal(rec$calls[[1]]$years, list("Agricultural fields" = 2023, "Land Use" = "2024", "Nitrogen" = "2025"))
    expect_equal(availability()$dataset, all_dataset_names)
    expect_match(as.character(output$availability$html), "available for the selected area", fixed = TRUE)
    expect_match(as.character(output$availability$html), "#2e7d32", fixed = TRUE)                      # green
    expect_match(as.character(output$availability$html), "Checked for: Own polygon", fixed = TRUE)
    expect_length(notes$notes, 0)

    # another selection: the marks and the sentence go, with a warning (yellow), and nothing is asked
    selected_polygons(selected_polygon("Other polygon"))
    session$flushReact()
    expect_null(availability())
    expect_null(output$availability$html)
    expect_length(rec$calls, 1)
    expect_length(notes$notes, 1)
    expect_equal(notes$notes[[1]][[2]], "warning")
    expect_match(notes$notes[[1]][[1]], "The selected area changed, so the availability of the datasets is no longer known. Click Check data availability",
                 fixed = TRUE)

    # no marks any more: another selection says nothing
    selected_polygons(NULL)
    session$flushReact()
    expect_length(notes$notes, 1)

    # the button checks the polygons that are selected then
    selected_polygons(selected_polygon("Third polygon"))
    session$setInputs(check_availability = 2)
    expect_length(rec$calls, 2)
    expect_match(as.character(output$availability$html), "#b26a00", fixed = TRUE)                      # orange: none found
    expect_match(as.character(output$availability$html), "Checked for: Third polygon", fixed = TRUE)
  })
})

test_that("a changed year or date clears the marks with a warning, but an input that appears is no change", {
  local_clean_cache()
  rec <- mock_check("yes")
  notes <- mock_notes()
  with_server({
    selected_polygons(selected_polygon())
    session$setInputs(selected_dataset = "Land Use")
    session$setInputs(landuse_year = "2024")                         # the input appears (the dataset is chosen)
    session$setInputs(check_availability = 1)
    expect_false(is.null(availability()))

    session$setInputs(selected_dataset = "Nitrogen")
    session$setInputs(nitrogen_year = "2025")                        # another input appears
    session$setInputs(selected_dataset = "Weather")
    session$setInputs(weather_mode = "single", weather_date = as.Date("2025-06-01"))
    expect_false(is.null(availability()))                            # none of these is a change
    expect_length(notes$notes, 0)

    session$setInputs(weather_date = as.Date("2025-06-02"))          # a date that had a value
    expect_null(availability())
    expect_length(notes$notes, 1)
    expect_equal(notes$notes[[1]][[2]], "warning")
    expect_match(notes$notes[[1]][[1]], "The year, date or distance changed, so the availability", fixed = TRUE)
    expect_length(rec$calls, 1)                                      # nothing was asked

    # every kind of year and date does it: the input gets a value (it appears), the datasets are checked, the value changes
    inputs <- list(landuse_year = list("2024", "2023"), nitrogen_year = list("2025", "2024"), selected_year = list(2023, 2022),
                   ndvi_mode = list("single", "range"), ndvi_year = list(2022, 2021), ndvi_month = list(6, 5),
                   ndvi_from_year = list(2020, 2019), ndvi_from_month = list(1, 2), ndvi_to_year = list(2021, 2022),
                   ndvi_to_month = list(12, 3), weather_mode = list("single", "period"),
                   weather_period = list(as.Date(c("2025-01-01", "2025-02-01")), as.Date(c("2025-01-01", "2025-03-01"))))
    clicks <- 1
    for (name in names(inputs)) {
      do.call(session$setInputs, setNames(list(inputs[[name]][[1]]), name))
      clicks <- clicks + 1
      session$setInputs(check_availability = clicks)
      expect_false(is.null(availability()), info = name)
      do.call(session$setInputs, setNames(list(inputs[[name]][[2]]), name))
      expect_null(availability(), info = name)
    }

    # the distance of the Weather settings is one too
    session$setInputs(check_availability = clicks + 1)
    session$setInputs(weather_max_km = 60)
    expect_null(availability())
  })
})

test_that("the marks that adding a dataset sets go with the next selection", {
  local_clean_cache()
  rec <- mock_check(list("yes"))
  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    selected_polygons(selected_polygon())
    session$flushReact()
    session$setInputs(add_dataset = 1)
    expect_equal(availability()$dataset, "Land Use")
    expect_equal(availability()$status, "yes")
    selected_polygons(selected_polygon("Other polygon"))
    session$flushReact()
    expect_null(availability())
    expect_length(rec$calls, 1)                           # only the check of the added dataset
  })
})

test_that("the button without an area checks nothing, and a check that cannot be made says so and clears the marks", {
  local_clean_cache()
  rec <- mock_check(list("yes", NULL))
  rec_notes <- new.env()
  rec_notes$notes <- character()
  local_mocked_bindings(showNotification = function(ui, ...) rec_notes$notes <- c(rec_notes$notes, ui))
  with_server({
    session$setInputs(selected_dataset = "Land Use", check_availability = 1)
    expect_length(rec$calls, 0)                           # the dialog says to select an area

    selected_polygons(selected_polygon())
    session$setInputs(check_availability = 2)
    expect_false(is.null(availability()))
    session$setInputs(check_availability = 3)
    expect_null(availability())                           # the check failed: no marks from before
    expect_match(rec_notes$notes, "could not be checked")
  })
})

test_that("adding a dataset where the services have no data for the polygon does not add it, and says what to change", {
  local_clean_cache()
  rec <- mock_check(list("no", "yes", "unknown"))
  rec_notes <- new.env()
  rec_notes$notes <- list()
  local_mocked_bindings(showNotification = function(ui, type = "default", ...) {
    rec_notes$notes[[length(rec_notes$notes) + 1]] <- list(ui, type)
  })
  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    selected_polygons(selected_polygon())
    session$flushReact()

    session$setInputs(add_dataset = 1)
    expect_equal(nrow(overview()), 0)                                 # not added
    expect_equal(rec_notes$notes[[length(rec_notes$notes)]][[2]], "error")
    expect_match(rec_notes$notes[[length(rec_notes$notes)]][[1]], "Land Use has no data for the selected area (Own polygon). Nothing was added: change the area, the year or the period", fixed = TRUE)
    expect_equal(rec$calls[[1]]$datasets, "Land Use")                 # only the dataset that is added
    expect_equal(rec$calls[[1]]$years, list("Land Use" = 2024L))
    expect_equal(availability()$status, "no")                          # and the menu shows it
    expect_match(as.character(output$availability$html), "has no data", fixed = TRUE)

    # data found: added
    session$setInputs(add_dataset = 2)
    expect_equal(overview()$dataset, "Land Use")
    expect_equal(availability()$status, "yes")

    # a check that could not tell does not stop it
    session$setInputs(landuse_year = "2023")
    session$setInputs(add_dataset = 3)
    expect_equal(overview()$year, c(2024L, 2023L))
  })
})

test_that("a check that fails does not stop a dataset from being added", {
  local_clean_cache()
  mock_check(list(NULL))
  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    selected_polygons(selected_polygon())
    session$setInputs(add_dataset = 1)
    expect_equal(overview()$dataset, "Land Use")
    expect_null(availability())
  })
})

test_that("with several polygons the ones without data are left out, and the others are added", {
  local_clean_cache()
  rec <- mock_check(list("yes", "no", "yes"))
  rec_notes <- new.env()
  rec_notes$notes <- list()
  local_mocked_bindings(showNotification = function(ui, type = "default", ...) {
    rec_notes$notes[[length(rec_notes$notes) + 1]] <- list(ui, type)
  })
  second <- selected_polygon("Area B")
  sf::st_geometry(second) <- sf::st_geometry(fixed_square(2, 5.9, 52.2))
  second$wkt <- sf::st_as_text(sf::st_geometry(second))
  both <- rbind(selected_polygon("Area A"), second)
  both$layer_id <- 1:2
  with_server({
    session$setInputs(selected_dataset = "Soil map")
    selected_polygons(both)
    session$setInputs(add_dataset = 1)
    expect_equal(rec$i, 2)                                            # one check for each polygon
    expect_equal(overview()$polygon, c("Area A", "Area B")[c(TRUE, FALSE)])
    expect_match(rec_notes$notes[[length(rec_notes$notes)]][[1]], "(Area B)", fixed = TRUE)
    expect_equal(availability()$status, "partial")                    # data for one of the two
    expect_match(availability()$detail, "found for 1 of 2 selected polygons")
  })
})

test_that("NDVI is checked for the view that is added", {
  local_clean_cache()
  rec <- mock_check(list("yes"))
  with_server({
    session$setInputs(selected_dataset = "NDVI", ndvi_mode = "single", ndvi_year = 2024, ndvi_month = 6, `ndvi_tab` = "Statistics")
    selected_polygons(selected_polygon())
    session$setInputs(ndvi_tab = "Geodata", add_dataset = 1)
    expect_equal(rec$calls[[1]]$view, "Geodata")
  })
})

test_that("the largest distance to the weather station can be set", {
  local_clean_cache()
  mock_services(distance = 80000)
  expect_equal(status_of_dataset(availability_check(area(), "t", "t"), "Weather"), "no")  # the default is 30 km
  far <- availability_check(area(), "t", "t", weather_max_km = 100)
  expect_equal(status_of_dataset(far, "Weather"), "yes")
  expect_match(far$detail[far$dataset == "Weather"], "A weather station is 80 km from")
  near <- availability_check(area(), "t", "t", weather_max_km = 79)
  expect_equal(status_of_dataset(near, "Weather"), "no")
  expect_match(near$detail[near$dataset == "Weather"], "the limit is 79 km")
})

test_that("the Weather settings have a box for the distance, with 30 km as the default", {
  with_server({
    session$setInputs(selected_dataset = "Weather")
    html <- as.character(build_controls_for("Weather", "Statistics"))
    expect_match(html, 'id="weather_max_km"', fixed = TRUE)
    expect_match(html, 'value="30"', fixed = TRUE)
    expect_match(html, "Largest distance to the weather station (km)", fixed = TRUE)
    expect_equal(weather_max_km(), 30)
  })
})

test_that("the distance of the box is used by the checks, a bad value is ignored, and the last good one is kept", {
  local_clean_cache()
  rec <- mock_check("yes")
  with_server({
    session$setInputs(selected_dataset = "Weather")
    selected_polygons(selected_polygon())
    session$setInputs(check_availability = 1)
    expect_equal(rec$calls[[1]]$km, 30)

    session$setInputs(weather_max_km = 75)
    expect_equal(weather_max_km(), 75)
    session$setInputs(check_availability = 2)
    expect_equal(rec$calls[[2]]$km, 75)                    # the new distance is used by the next check

    for (bad in list(NA, -5, 0, "abc", NULL)) {
      session$setInputs(weather_max_km = bad)
      expect_equal(weather_max_km(), 75)  # the last good value stays
    }

    # the box is built again with the value that was set (it is gone while another dataset is chosen)
    session$setInputs(selected_dataset = "AHN")
    expect_match(as.character(build_controls_for("Weather", "Statistics")), 'value="75"', fixed = TRUE)
  })
})

test_that("the button is called Check data availability, and the info button next to the datasets explains the marks", {
  html <- as.character(app_ui())
  expect_match(html, '<button[^>]*id="check_availability"[^>]*>\\s*Check data availability\\s*</button>')
  expect_false(grepl("which datasets have data for the selected area", html, fixed = TRUE))   # (the grey text is gone)
  expect_false(grepl("Click Check data availability to mark the datasets", html, fixed = TRUE))
  expect_match(html, '<button[^>]*btn-info-circle[^>]*id="info_availability"')

  rec <- new.env()
  rec$shown <- NULL
  local_mocked_bindings(showModal = function(ui, ...) rec$shown <- ui)
  with_server({
    session$setInputs(info_availability = 1)
    text <- as.character(rec$shown)
    expect_match(text, "Data availability", fixed = TRUE)
    expect_match(text, "Check data availability", fixed = TRUE)
    expect_match(text, "data found", fixed = TRUE)
    expect_match(text, "A polygon with no data for the dataset is", fixed = TRUE)
    expect_match(text, "next to", fixed = TRUE)
  })
})

test_that("Check data availability starts disabled, and is enabled when polygons are selected", {
  html <- as.character(app_ui())
  tag <- regmatches(html, regexpr('<button[^>]*id="check_availability"[^>]*>', html))
  expect_match(tag, ' disabled ', fixed = TRUE)                                  # disabled from the start

  with_server({
    session$flushReact()
    expect_false(check_enabled())                                    # nothing selected

    # a project, an upload or a drawn polygon that is there but not selected is no reason
    fixed_polys(fixed_square(1, 5.7, 52))
    uploaded_polys(fixed_square(2, 5.8, 52, name = "Upload_1"))
    drawn_features(fixed_square(3, 5.9, 52, name = "Own polygon_1"))
    session$flushReact()
    expect_false(check_enabled())

    selected_polygons(selected_polygon())                            # a polygon is selected
    session$flushReact()
    expect_true(check_enabled())
    selected_polygons(NULL)                                          # deselected
    session$flushReact()
    expect_false(check_enabled())
  })
})

test_that("the check reports how many datasets have been tested, from 0 up to the total", {
  local_clean_cache()
  rec <- mock_services()
  done <- list()
  availability_check(area(), "t", "t", years = list("Agricultural fields" = 2023),
                     progress = function(done_n, total) done[[length(done) + 1]] <<- c(done_n, total))
  expect_equal(done[[1]], c(0, 9))
  expect_equal(done[[length(done)]], c(9, 9))
  expect_equal(length(done), 10)                                     # 0 and then each of the 9 datasets
  expect_equal(vapply(done, function(d) d[1], 0), 0:9)

  done <- list()
  availability_check(area(), "t", "t", datasets = c("AHN", "Weather"),
                     progress = function(done_n, total) done[[length(done) + 1]] <<- c(done_n, total))
  expect_equal(done, list(c(0, 2), c(1, 2), c(2, 2)))
})

test_that("the progress bar says Checking datasets and how many of them have been tested", {
  local_clean_cache()
  mock_check("yes")
  seen <- new.env()
  seen$set <- list()
  local_mocked_bindings(setProgress = function(value = NULL, message = NULL, detail = NULL, ...) {
    seen$set[[length(seen$set) + 1]] <- list(value = value, detail = detail)
  })
  with_server({
    selected_polygons(selected_polygon())
    session$setInputs(check_availability = 1)
    shown <- Filter(function(x) grepl("of 9 datasets tested", x$detail), seen$set)         # (the dialog has its own bar)
    expect_length(shown, 10)
    expect_equal(shown[[1]], list(value = 0, detail = "0 of 9 datasets tested"))
    expect_equal(shown[[5]], list(value = 4 / 9, detail = "4 of 9 datasets tested"))
    expect_equal(shown[[10]], list(value = 1, detail = "9 of 9 datasets tested"))
  })
  expect_match(paste(deparse(body(server_availability)), collapse = " "), "Checking datasets...", fixed = TRUE)
})

test_that("adding a dataset shows how many of the polygons have been tested", {
  local_clean_cache()
  mock_check("yes")
  seen <- new.env()
  seen$set <- list()
  local_mocked_bindings(setProgress = function(value = NULL, message = NULL, detail = NULL, ...) {
    seen$set[[length(seen$set) + 1]] <- detail
  })
  second <- selected_polygon("Area B")
  sf::st_geometry(second) <- sf::st_geometry(fixed_square(2, 5.9, 52.2))
  second$wkt <- sf::st_as_text(sf::st_geometry(second))
  both <- rbind(selected_polygon("Area A"), second)
  with_server({
    session$setInputs(selected_dataset = "Soil map")
    selected_polygons(both)
    session$setInputs(add_dataset = 1)
    expect_equal(Filter(function(x) grepl("polygons tested", x), unlist(seen$set)), c("0 of 2 polygons tested", "1 of 2 polygons tested"))
  })
})

test_that("NDVI rasters for a period: the days with data, none is no data, and a failure is unknown", {
  local_clean_cache()
  mock_services()
  days <- 4
  asked <- new.env()
  asked$n <- 0
  local_mocked_bindings(ndc_count = function(collection, roi, trange, limit, token) {
    asked$n <- asked$n + 1
    asked$args <- list(collection = collection, trange = trange, token = token)
    if (is.null(days)) stop("down")
    days
  }, .package = "rNDC")
  geometry <- sf::st_union(sf::st_geometry(area()))
  period <- as.Date(c("2024-06-01", "2024-06-30"))

  found <- availability_ndvi(geometry, area(), "Geodata", period, "tok")
  expect_equal(found$status, "yes")
  expect_equal(found$detail, "NDVI rasters: 4 days with data between 2024-06-01 and 2024-06-30 for the selected area.")
  expect_equal(asked$args, list(collection = "ndvi", trange = c("2024-06-01", "2024-06-30"), token = "tok"))
  availability_ndvi(geometry, area(), "Geodata", period, "tok")
  expect_equal(asked$n, 1)                                              # asked once for the same area and period

  days <- 1
  expect_match(availability_ndvi(geometry, area(), "Geodata", as.Date(c("2024-07-01", "2024-07-31")), "tok")$detail,
               "NDVI rasters: 1 day with data between 2024-07-01 and 2024-07-31", fixed = TRUE)
  days <- 0
  none <- availability_ndvi(geometry, area(), "Geodata", as.Date(c("2010-01-01", "2010-12-31")), "tok")
  expect_equal(none$status, "no")
  expect_equal(none$detail, "No NDVI rasters between 2010-01-01 and 2010-12-31 for the selected area.")
  days <- NULL
  down <- availability_ndvi(geometry, area(), "Geodata", as.Date(c("2011-01-01", "2011-12-31")), "tok")
  expect_equal(down$status, "unknown")
  expect_match(down$detail, "Could not check for NDVI rasters")

  # outside the Netherlands: no, without asking
  asked$n <- 0
  abroad <- area("Own polygon_1", x = 4.6, y = 50.6)
  out <- availability_ndvi(sf::st_union(sf::st_geometry(abroad)), abroad, "Geodata", period, "tok")
  expect_equal(c(out$status, asked$n), c("no", 0))
  expect_match(out$detail, "outside the Netherlands")
})

test_that("NDVI statistics for a period: the observations of the project polygons, none is no data, other areas have none", {
  local_clean_cache()
  mock_services()
  stats <- tibble::tibble(ndc_id = "1", month = c("2024-05", "2024-06", "2024-06"), ndvi_mean = 0.5, ndvi_std = 0.1, n_obs = c(2L, 3L, 1L))
  asked <- new.env()
  asked$n <- 0
  local_mocked_bindings(ndc_ndvi_stats = function(roi, collection, trange, token, ...) {
    asked$n <- asked$n + 1
    asked$args <- list(collection = collection, trange = trange, token = token)
    if (is.null(stats)) stop("down")
    stats
  }, .package = "rNDC")
  project <- area("Nestboxes_3")
  geometry <- sf::st_union(sf::st_geometry(project))
  period <- as.Date(c("2024-05-01", "2024-06-30"))

  found <- availability_ndvi(geometry, project, "Statistics", period, "tok")
  expect_equal(found$status, "yes")
  expect_equal(found$detail, "NDVI statistics: 6 observations in 2 months between 2024-05-01 and 2024-06-30 for Nestboxes_3.")
  expect_equal(asked$args, list(collection = "ndvi-lter", trange = c("2024-05-01", "2024-06-30"), token = "tok"))
  availability_ndvi(geometry, project, "Statistics", period, "tok")
  expect_equal(asked$n, 1)                                                # cached

  snl <- area("SNL parcel_2")
  availability_ndvi(sf::st_union(sf::st_geometry(snl)), snl, "Statistics", period, "tok")
  expect_equal(asked$args$collection, "ndvi-snl")

  stats <- stats[0, ]
  none <- availability_ndvi(geometry, project, "Statistics", as.Date(c("2010-01-01", "2010-12-31")), "tok")
  expect_equal(none$status, "no")
  expect_equal(none$detail, "No NDVI statistics between 2010-01-01 and 2010-12-31 for Nestboxes_3.")
  stats <- NULL
  expect_equal(availability_ndvi(geometry, project, "Statistics", as.Date(c("2011-01-01", "2011-12-31")), "tok")$status, "unknown")

  # an area that is no project area has no statistics, whatever the period (and nothing is asked)
  asked$n <- 0
  own <- area("Own polygon_1")
  no_project <- availability_ndvi(sf::st_union(sf::st_geometry(own)), own, "Statistics", period, "tok")
  expect_equal(c(no_project$status, asked$n), c("no", 0))
  expect_equal(no_project$detail, "No NDVI statistics: they exist only for LTER and SNL project areas.")
})

test_that("the NDVI of the check next to Add uses the dates and the view that are added", {
  local_clean_cache()
  mock_services()
  local_mocked_bindings(ndc_count = function(...) 4, .package = "rNDC")
  with_server({
    session$setInputs(selected_dataset = "NDVI", ndvi_mode = "single", ndvi_year = 2024, ndvi_month = 6, ndvi_tab = "Geodata")
    selected_polygons(selected_polygon())
    session$setInputs(check_dataset = 1)
    row <- availability_of(availability(), "NDVI")
    expect_equal(row$status, "yes")
    expect_equal(row$detail, "NDVI rasters: 4 days with data between 2024-06-01 and 2024-06-30 for the selected area.")
    expect_true(add_enabled())

    # a month with no rasters: no data, so Add stays disabled
    local_mocked_bindings(ndc_count = function(...) 0, .package = "rNDC")
    session$setInputs(ndvi_month = 7)
    session$setInputs(check_dataset = 2)
    expect_equal(availability_of(availability(), "NDVI")$status, "no")
    expect_match(availability_of(availability(), "NDVI")$detail, "^No NDVI rasters between 2024-07-01 and 2024-07-31")
    expect_false(add_enabled())
  })
})

test_that("Weather for the dates asked for: the days with observations, none is no data with the start of the series, and the distance first", {
  local_clean_cache()
  rec <- mock_services()
  geometry <- sf::st_union(sf::st_geometry(area()))
  period <- as.Date(c("2024-06-01", "2024-06-07"))

  found <- availability_weather(geometry, "k1", "t", period = period)
  expect_equal(found$status, "yes")
  expect_equal(found$detail, "Weather: station 275 (12 km away) has observations on 7 days between 2024-06-01 and 2024-06-07.")
  expect_equal(rec$meteo[[1]][["meteostation"]], "275")
  expect_equal(unname(rec$meteo[[1]][c("fromdate", "todate", "page_size")]), c("20240601", "20240607", "500"))
  n <- length(rec$calls)
  availability_weather(geometry, "k1", "t", period = period)
  expect_length(rec$calls, n)                                          # cached

  # one day, and a long period (a page of 500 days is all that is asked)
  expect_match(availability_weather(geometry, "k1", "t", period = as.Date(c("2024-06-01", "2024-06-01")))$detail,
               "has observations on 1 day between 2024-06-01 and 2024-06-01.", fixed = TRUE)
  expect_match(availability_weather(geometry, "k1", "t", period = as.Date(c("2000-01-01", "2024-06-01")))$detail,
               "has observations on at least 500 days between 2000-01-01 and 2024-06-01.", fixed = TRUE)

  # no observations in the period: no data, and when the station starts
  rec$observed <- function(params) if (params[["fromdate"]] == "19000101") "1960-01-03" else character(0)
  none <- availability_weather(geometry, "k1", "t", period = as.Date(c("1950-01-01", "1950-01-07")))
  expect_equal(none$status, "no")
  expect_equal(none$detail, "Weather: station 275 (12 km away) has no observations between 1950-01-01 and 1950-01-07. Its observations start on 1960-01-03.")
  local_clean_cache()                                                  # (the start of the series is remembered per station)
  rec$observed <- function(params) character(0)
  expect_equal(availability_weather(geometry, "k1", "t", period = as.Date(c("1951-01-01", "1951-01-07")))$detail,
               "Weather: station 275 (12 km away) has no observations between 1951-01-01 and 1951-01-07.")

  # the request fails: unknown
  local_mocked_bindings(adc_get = function(...) stop("down"), .package = "rNDC")
  down <- availability_weather(geometry, "k1", "t", period = as.Date(c("2020-01-01", "2020-01-07")))
  expect_equal(down$status, "unknown")
  expect_match(down$detail, "Could not check for Weather observations")

  # too far: the distance says so, and the observations are not asked
  local_clean_cache()
  rec2 <- mock_services(distance = 80000)
  far <- availability_weather(geometry, "k2", "t", period = period)
  expect_equal(far$status, "no")
  expect_match(far$detail, "closest weather station is 80 km from the selected area", fixed = TRUE)
  expect_false("Meteo_data" %in% rec2$calls)
})

test_that("Agricultural fields: the year, and the years that the register has when there are none", {
  local_clean_cache()
  mock_services()
  geometry <- sf::st_union(sf::st_geometry(area()))
  expect_equal(availability_fields(geometry, "kf", "t", 2023)$detail, "Agricultural fields 2023: fields found in the selected area.")
  local_clean_cache()
  mock_services(fields = 0)
  none <- availability_fields(geometry, "kf", "t", 2008)
  expect_equal(none$status, "no")
  last <- as.integer(format(Sys.Date(), "%Y")) - 1L
  expect_equal(none$detail, paste0("No agricultural fields in 2008 for the selected area (the register has fields from 2009 to ", last, ")."))
})

test_that("the check of a dataset with the dates it is added for passes them on to Weather", {
  local_clean_cache()
  rec <- mock_services()
  with_server({
    selected_polygons(selected_polygon())
    session$setInputs(selected_dataset = "Weather", weather_mode = "period", weather_period = as.Date(c("2024-06-01", "2024-06-10")))
    session$setInputs(check_dataset = 1)
    expect_match(availability_of(availability(), "Weather")$detail, "has observations on 10 days between 2024-06-01 and 2024-06-10", fixed = TRUE)
    expect_true(add_enabled())

    rec$observed <- function(params) character(0)
    session$setInputs(weather_period = as.Date(c("2024-07-01", "2024-07-10")))
    session$setInputs(check_dataset = 2)
    expect_equal(availability_of(availability(), "Weather")$status, "no")
    expect_false(add_enabled())                                        # no observations: no data
  })
})
