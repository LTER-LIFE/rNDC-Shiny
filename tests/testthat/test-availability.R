# "Is there data for the selected area?": counts of STAC items, the check of every dataset and the marks in the app.

test_that("count_items returns the number of matching items, asks for the area and the period, and caches", {
  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"), raster_item("b", "2024-01-01"))))

  n <- count_items("lgn", sf::st_geometry(selected_polygon()), "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z")
  expect_equal(n, 2)
  body <- last_request_body()
  expect_equal(unlist(body$collections), "lgn")
  expect_equal(body$intersects$type, "Polygon")
  expect_equal(body$datetime, "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z")

  n_requests <- length(request_uris())
  expect_equal(count_items("lgn", sf::st_geometry(selected_polygon()), "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z"), 2)
  expect_length(request_uris(), n_requests)  # from the cache

  # another period is another question
  local_stac_api(list(list()))
  expect_equal(count_items("lgn", sf::st_geometry(selected_polygon()), "2025-01-01T00:00:00Z/2025-12-31T23:59:59Z"), 0)
})

test_that("count_items gives NA, and does not cache, when the request fails", {
  local_clean_cache()
  local_stac_api(list(list()), status = 500L)
  expect_true(is.na(count_items("lgn", sf::st_geometry(selected_polygon()))))
  expect_length(ls(ndc_cache), 0)
})


test_that("raster availability depends on the year and names the missing layers", {
  table <- data.frame(collection = c("ntot", "nox", "ntot"), year = c("2024", "2024", "2025"), n_items = c(1, 2, 1))
  layers <- c("ntot", "nox", "nh3")

  expect_equal(availability_raster("Nitrogen", layers, table, "2024")$status, "partial")
  expect_equal(availability_raster("Nitrogen", layers, table, "2024")$detail,
               "No Nitrogen data for 2024 found for the selected area (nh3).")
  expect_equal(availability_raster("Nitrogen", c("ntot", "nox"), table, "2024")$status, "yes")
  expect_equal(availability_raster("Nitrogen", c("ntot", "nox"), table, "2024")$detail,
               "Nitrogen data for 2024 is available for the selected area.")
  expect_equal(availability_raster("Nitrogen", layers, table, "2031")$status, "no")
  expect_equal(availability_raster("Nitrogen", layers, table, "2031")$detail,
               "No Nitrogen data for 2031 found for the selected area (ntot, nox, nh3).")
  # without a year any year will do
  expect_equal(availability_raster("Nitrogen", "ntot", table, NULL)$detail, "Nitrogen data is available for the selected area.")
  # a collection that was asked separately cannot say which years: it counts for any year
  separate <- data.frame(collection = "lgn", year = NA_character_, n_items = 5)
  expect_equal(availability_raster("Land Use", "lgn", separate, "2024")$status, "yes")
  # nothing found / the check failed
  expect_equal(availability_raster("Land Use", "lgn", table[0, ], "2024")$status, "no")
  expect_equal(availability_raster("Land Use", "lgn", NULL, "2024")$status, "unknown")
  failed <- data.frame(collection = "lgn", year = NA_character_, n_items = NA_real_)
  expect_equal(availability_raster("Land Use", "lgn", failed, "2024")$status, "unknown")
})

# the services, answering as they do in the Netherlands
mock_services <- function(env = parent.frame(), soil = 1, ahn = list(area = 1, min = 1), fields = 1, distance = 12000,
                          stac = data.frame(collection = c("lgn", "ntot", "nox", "nh3"), year = "2024", n_items = 1)) {
  rec <- new.env()
  rec$calls <- character(0)
  testthat::local_mocked_bindings(
    adc_get = function(url = NULL, option = NULL, params = NULL, server = "adc", token, ...) {
      if (!is.null(option)) {
        rec$calls <- c(rec$calls, option)
        rec$params <- params
        n <- switch(option, Soiltypes = soil, Fields = fields)
        return(list(features = if (n > 0) lapply(seq_len(n), function(i) list(properties = list(a = 1))) else list()))
      }
      rec$calls <- c(rec$calls, "AHN")
      list(features = list(list(properties = ahn)))
    },
    adc_weather_station = function(roi, token, ...) {
      rec$calls <- c(rec$calls, "stations")
      list(distances = c(distance + 5000, distance))
    },
    .package = "rNDC", .env = env
  )
  testthat::local_mocked_bindings(
    stac_availability = function(collections, roi, token) {
      rec$calls <- c(rec$calls, "stac")
      stac
    },
    .package = "rNDC", .env = env
  )
  rec
}

area <- function(name = "Own polygon", x = 5.66, y = 51.99) {
  sf::st_sf(source_name = name, wkt = sf::st_as_text(sf::st_geometry(fixed_square(1, x, y))),
            geometry = sf::st_geometry(fixed_square(1, x, y)))
}

status_of_dataset <- function(table, dataset) table$status[table$dataset == dataset]

test_that("every dataset gets a status for an area in the Netherlands", {
  local_clean_cache()
  mock_services()
  table <- check_area_availability(area(), "t", "t", years = list("Agricultural fields" = 2023, "Land Use" = "2024", "Nitrogen" = "2024"))

  expect_equal(table$dataset, all_dataset_names)
  expect_equal(unname(setNames(table$status, table$dataset)[c("Weather", "Nitrogen", "Soil map", "AHN", "Agricultural fields", "Land Use")]),
               rep("yes", 6))
  expect_equal(status_of_dataset(table, "NDVI"), "yes")  # the rasters cover the Netherlands
  expect_equal(status_of_dataset(table, "Vegetation structure"), "na")
  expect_equal(status_of_dataset(table, "Ground water table"), "na")
  expect_match(table$detail[table$dataset == "Weather"], "A weather station is 12 km from the selected area.")
  expect_match(table$detail[table$dataset == "Agricultural fields"], "for 2023 are available")
})

test_that("the services say no, and the check says so", {
  local_clean_cache()
  mock_services(soil = 0, ahn = list(), fields = 0, distance = 80000, stac = data.frame(collection = character(0), year = character(0), n_items = numeric(0)))
  table <- check_area_availability(area(), "t", "t", years = list("Agricultural fields" = 2023, "Land Use" = "2024", "Nitrogen" = "2024"))
  for (dataset in c("Weather", "Nitrogen", "Soil map", "AHN", "Agricultural fields", "Land Use")) {
    expect_equal(status_of_dataset(table, dataset), "no", info = dataset)
  }
  expect_match(table$detail[table$dataset == "Weather"], "closest weather station is 80 km from the selected area (the limit is 30 km).", fixed = TRUE)
  expect_match(table$detail[table$dataset == "AHN"], "No elevation")
})

test_that("failed requests are unknown, not no, and are not cached", {
  local_clean_cache()
  rec <- mock_services()
  local_mocked_bindings(adc_get = function(...) stop("down"), adc_weather_station = function(...) stop("down"),
                        .package = "rNDC")
  local_mocked_bindings(stac_availability = function(...) stop("down"), .package = "rNDC")
  table <- check_area_availability(area(), "t", "t", years = list("Agricultural fields" = 2023))
  for (dataset in c("Weather", "Nitrogen", "Soil map", "AHN", "Agricultural fields", "Land Use")) {
    expect_equal(status_of_dataset(table, dataset), "unknown", info = dataset)
  }
  expect_length(ls(ndc_cache), 0)
})

test_that("without an AgroDataCube token its datasets do not apply, and Fields wants a year", {
  local_clean_cache()
  rec <- mock_services()
  table <- check_area_availability(area(), "t", "", years = list())
  for (dataset in c("Weather", "Soil map", "AHN", "Agricultural fields")) {
    expect_equal(status_of_dataset(table, dataset), "na", info = dataset)
  }
  expect_false(any(c("AHN", "Soiltypes", "stations", "Fields") %in% rec$calls))
  expect_equal(status_of_dataset(table, "Land Use"), "yes")

  table <- check_area_availability(area(), "t", "t", years = list())
  expect_equal(status_of_dataset(table, "Agricultural fields"), "unknown")
  expect_match(table$detail[table$dataset == "Agricultural fields"], "Choose a year")
})

test_that("NDVI: rasters for the Netherlands, statistics for project areas", {
  local_clean_cache()
  mock_services()
  drawn <- check_area_availability(area("Own polygon_1"), "t", "t")
  expect_equal(status_of_dataset(drawn, "NDVI"), "yes")
  expect_match(drawn$detail[drawn$dataset == "NDVI"], "rasters for the selected area; no statistics")

  project <- check_area_availability(area("Nestboxes_3"), "t", "t")
  expect_match(project$detail[project$dataset == "NDVI"], "statistics for 1 of 1 selected polygon")

  abroad <- check_area_availability(area("Own polygon_1", 4.6, 50.6), "t", "t")  # Belgium
  expect_equal(status_of_dataset(abroad, "NDVI"), "no")
  expect_match(abroad$detail[abroad$dataset == "NDVI"], "outside the Netherlands")

  # a project area outside the outline (it is not asked for) still has statistics
  expect_equal(status_of_dataset(check_area_availability(area("SNL parcel_2", 4.6, 50.6), "t", "t"), "NDVI"), "yes")
})

test_that("the checks are cached per area, and Fields per year", {
  local_clean_cache()
  rec <- mock_services()
  years <- list("Agricultural fields" = 2023, "Land Use" = "2024", "Nitrogen" = "2024")
  check_area_availability(area(), "t", "t", years)
  first <- length(rec$calls)
  expect_equal(sum(rec$calls == "stac"), 1)  # one search for Land Use and Nitrogen

  check_area_availability(area(), "t", "t", years)
  expect_length(rec$calls, first)  # everything from the cache

  years[["Land Use"]] <- "2025"
  check_area_availability(area(), "t", "t", years)
  expect_length(rec$calls, first)  # the year of a raster needs no new request

  years[["Agricultural fields"]] <- 2022
  check_area_availability(area(), "t", "t", years)
  expect_length(rec$calls, first + 1)  # only Fields
  expect_equal(rec$calls[length(rec$calls)], "Fields")

  check_area_availability(area(x = 5.9), "t", "t", years)  # another area
  expect_gt(length(rec$calls), first + 1)
})

test_that("a long polygon is shortened for AgroDataCube like in a retrieval", {
  local_clean_cache()
  rec <- mock_services()
  a <- seq(0, 2 * pi, length.out = 801)
  r <- 0.003 * (1 + 0.05 * sin(7 * a))
  geom <- sf::st_sfc(sf::st_polygon(list(cbind(5.66 + r * cos(a), 51.99 + r * sin(a)))), crs = 4326)
  long <- sf::st_sf(source_name = "Own polygon_1", geometry = geom)
  check_area_availability(long, "t", "t", years = list("Agricultural fields" = 2023))
  expect_lte(nchar(rec$params[["geometry"]]), adc_max_wkt_chars)
})

test_that("the row of a dataset, and the warning for one without data", {
  table <- data.frame(dataset = c("AHN", "Weather"), status = c("no", "yes"), detail = c("No elevation (AHN) data found for the selected area.", "ok"))
  expect_equal(availability_of(table, "AHN")$status, "no")
  expect_null(availability_of(table, "Land Use"))
  expect_null(availability_of(NULL, "AHN"))
  expect_equal(no_data_warning(availability_of(table, "AHN")),
               "No elevation (AHN) data found for the selected area. It is added anyway: the services may still have data for it.")
})

# ---- in the app ----

test_that("the app checks the selected area, and says what it found for the selected dataset", {
  local_clean_cache()
  rec <- new.env()
  local_mocked_bindings(check_area_availability = function(area, ndc_token, adc_token, years = list(), weather_max_km = 30) {
    rec$years <- years
    rec$km <- weather_max_km
    rec$n <- if (is.null(rec$n)) 1 else rec$n + 1
    rbind(availability_row("Land Use", "yes", "Land Use data for 2024 is available for the selected area."),
          availability_row("AHN", "no", "No elevation (AHN) data found for the selected area."),
          availability_row("Weather", "unknown", "Could not check for a weather station."))
  })
  sel <- selected_polygon()

  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024", nitrogen_year = "2025", selected_year = 2023)
    expect_null(availability())
    expect_null(output$availability$html)

    selected_polygons(sel)
    session$flushReact()
    expect_null(availability())  # after a pause, so that polygons can be selected one after another
    session$elapse(500)
    expect_equal(availability()$status, c("yes", "no", "unknown"))
    expect_equal(rec$years, list("Agricultural fields" = 2023, "Land Use" = "2024", "Nitrogen" = "2025"))
    expect_match(as.character(output$availability$html), "Land Use data for 2024 is available for the selected area.",
                 fixed = TRUE)
    expect_match(as.character(output$availability$html), "#2e7d32", fixed = TRUE)  # green

    session$setInputs(selected_dataset = "AHN")
    expect_match(as.character(output$availability$html), "No elevation (AHN) data found", fixed = TRUE)
    expect_match(as.character(output$availability$html), "#b26a00", fixed = TRUE)

    session$setInputs(selected_dataset = "Weather")
    expect_null(output$availability$html)  # unknown: say nothing

    # the selection ends: the result goes
    selected_polygons(NULL)
    session$flushReact()
    session$elapse(500)
    expect_null(availability())
  })
})

test_that("a changed year is checked again, and a failed check says nothing", {
  local_clean_cache()
  rec <- new.env()
  rec$n <- 0
  local_mocked_bindings(check_area_availability = function(area, ndc_token, adc_token, years = list(), weather_max_km = 30) {
    rec$n <- rec$n + 1
    if (rec$n == 2) stop("boom")
    availability_row("Land Use", "yes", paste("Land Use data for", years[["Land Use"]]))
  })

  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    selected_polygons(selected_polygon())
    session$flushReact()
    session$elapse(500)
    expect_match(availability()$detail, "2024")

    session$setInputs(landuse_year = "2025")
    session$elapse(500)
    expect_null(availability())  # the check failed
    expect_null(output$availability$html)
    expect_equal(rec$n, 2)
  })
})

test_that("adding a dataset where no data was found warns, and adds it anyway", {
  local_clean_cache()
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))
  local_mocked_bindings(check_area_availability = function(...) {
    rbind(availability_row("Land Use", "no", "No Land Use data for 2024 found for the selected area."),
          availability_row("Nitrogen", "yes", "Nitrogen data is available for the selected area."))
  })

  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    selected_polygons(selected_polygon())
    session$flushReact()
    session$elapse(500)

    session$setInputs(add_dataset = 1)
    expect_equal(overview()$dataset, "Land Use")  # added
    expect_true(any(grepl("It is added anyway", rec$notes)))

    rec$notes <- character(0)
    session$setInputs(selected_dataset = "Nitrogen", nitrogen_year = "2024")
    session$setInputs(add_dataset = 2)
    expect_equal(overview()$dataset, c("Land Use", "Nitrogen"))
    expect_false(any(grepl("added anyway", rec$notes)))
  })
})

test_that("the token is passed on to the counts and the years", {
  local_clean_cache()
  seen <- character()
  local_mocked_bindings(ndc_count = function(..., token) { seen <<- c(seen, token); 1 }, .package = "rNDC")
  expect_equal(count_items("lgn", sf::st_geometry(selected_polygon()), token = "mine"), 1)
  expect_equal(seen, "mine")

  local_mocked_bindings(ndc_landuse_years = function(token) { seen <<- c(seen, token); "2024" }, .package = "rNDC")
  expect_equal(get_landuse_years("mine2"), "2024")
  expect_equal(seen, c("mine", "mine2"))
})

test_that("the largest distance to the weather station can be set", {
  local_clean_cache()
  mock_services(distance = 80000)
  expect_equal(status_of_dataset(check_area_availability(area(), "t", "t"), "Weather"), "no")  # the default is 30 km
  far <- check_area_availability(area(), "t", "t", weather_max_km = 100)
  expect_equal(status_of_dataset(far, "Weather"), "yes")
  expect_match(far$detail[far$dataset == "Weather"], "A weather station is 80 km from")
  near <- check_area_availability(area(), "t", "t", weather_max_km = 79)
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

test_that("the distance of the box is used by the check, a bad value is ignored, and the last good one is kept", {
  local_clean_cache()
  rec <- new.env()
  rec$km <- numeric(0)
  local_mocked_bindings(check_area_availability = function(area, ndc_token, adc_token, years = list(), weather_max_km = 30) {
    rec$km <- c(rec$km, weather_max_km)
    availability_row("Weather", "yes", "x")
  })

  with_server({
    session$setInputs(selected_dataset = "Weather")
    selected_polygons(selected_polygon())
    session$flushReact()
    session$elapse(500)
    expect_equal(rec$km, 30)

    session$setInputs(weather_max_km = 75)
    session$elapse(500)
    expect_equal(weather_max_km(), 75)
    expect_equal(rec$km, c(30, 75))  # checked again with the new distance

    for (bad in list(NA, -5, 0, "abc", NULL)) {
      session$setInputs(weather_max_km = bad)
      session$elapse(500)
      expect_equal(weather_max_km(), 75)  # the last good value stays
    }

    # the box is built again with the value that was set (it is gone while another dataset is chosen)
    session$setInputs(selected_dataset = "AHN")
    expect_match(as.character(build_controls_for("Weather", "Statistics")), 'value="75"', fixed = TRUE)
  })
})
