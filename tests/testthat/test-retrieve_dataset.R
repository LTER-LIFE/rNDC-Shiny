# The retrieval functions of the datasets, tested without a Shiny session. (R/retrieve_dataset.R)

date <- as.Date

test_that("outcomes describe either data or why there is none", {
  out <- retrieve_outcome_data(1:3, "Thing", "csv", ok_msg = "ok", fail_msg = "fail")
  expect_equal(out$name, "Thing")
  expect_equal(out$ext, "csv")
  expect_true(is.function(out$writer))

  none <- retrieve_outcome_none("msg", "tif")
  expect_null(none$data)
  expect_equal(none$status, "failed")
  expect_equal(retrieve_outcome_none("msg", NA_character_, status = "skipped")$status, "skipped")
})

test_that("vector datasets are tables in the Statistics view and GeoPackages otherwise", {
  expect_equal(retrieve_vector_ext(overview_row("AHN", "Statistics")), "csv")
  expect_equal(retrieve_vector_ext(overview_row("AHN", "statistics")), "csv")
  expect_equal(retrieve_vector_ext(overview_row("Soil map", "Geodata")), "gpkg")
})

test_that("retrieve_dataset reports datasets that have no retrieval", {
  out <- retrieve_dataset(overview_row("Vegetation structure"), 1, "t", "t")
  expect_null(out$data)
  expect_equal(out$status, "skipped")
  expect_match(out$message, "Vegetation structure is not wired")
})

test_that("weather: the reasons for having no data are reported", {
  station_id <- NULL
  local_mocked_bindings(
    adc_weather_station = function(...) list(closest_id = station_id),
    adc_weather_date = function(...) NULL,
    .package = "rNDC"
  )
  dated <- overview_row("Weather", "Statistics", from = date("2024-05-01"), to = date("2024-05-01"))

  station_id <- character(0)
  expect_equal(retrieve_weather(dated, "t")$message, "Failed: Weather - no nearby station found")
  station_id <- NA_character_
  expect_equal(retrieve_weather(dated, "t")$message, "Failed: Weather - no nearby station found")
  station_id <- c("1", "2")
  expect_equal(retrieve_weather(dated, "t")$message, "Failed: Weather - no nearby station found")

  station_id <- "260"
  expect_equal(retrieve_weather(overview_row("Weather", "Statistics"), "t")$message,
               "Failed: Weather - missing date range")
  out <- retrieve_weather(dated, "t")
  expect_equal(out$message, "Failed: Weather - no data returned")
  expect_equal(out$file_type, "csv")  # the file type follows the view
  expect_equal(retrieve_weather(modifyList(dated, list(view = "Geodata")), "t")$file_type, "gpkg")
})

test_that("weather: one date uses adc_weather_date and a period the long-period function", {
  calls <- character()
  meteo <- sf::st_sf(day = 1, geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326))
  local_mocked_bindings(
    adc_weather_station = function(...) list(closest_id = "260"),
    adc_weather_date = function(id, date, token, ...) { calls <<- c(calls, paste("date", id, format(date), token)); meteo },
    adc_weather_long_period = function(station, trange, token, by_days, sleep_sec, ...) {
      calls <<- c(calls, paste("period", station, format(trange[1]), format(trange[2]), by_days))
      meteo
    },
    .package = "rNDC"
  )
  one <- retrieve_weather(overview_row("Weather", "Statistics", from = date("2024-05-01"), to = date("2024-05-01")), "tok")
  many <- retrieve_weather(overview_row("Weather", "Statistics", from = date("2024-05-01"), to = date("2024-05-09")), "tok")
  expect_equal(calls, c("date 260 2024-05-01 tok", "period 260 2024-05-01 2024-05-09 200"))
  expect_equal(one$name, "Weather")
  expect_equal(many$ok_msg, "Retrieved: Weather")
})

test_that("rasters: land use falls back to the default year, and messages name the year", {
  years <- NULL
  local_mocked_bindings(
    ndc_landuse_raster = function(roi, year, ...) { years <<- c(years, year); list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4)) },
    ndc_nitrogen_raster = function(roi, year, layers, ...) list(stack = NULL),
    .package = "rNDC"
  )
  lu <- retrieve_land_use(overview_row("Land Use"), "t")
  expect_equal(years, 2024L)
  expect_equal(lu$ok_msg, "Retrieved: Land Use raster for year 2024")
  expect_equal(lu$fail_msg, "Failed: Land Use - could not write output")
  expect_equal(lu$ext, "tif")

  retrieve_land_use(overview_row("Land Use", year = 2020L), "t")
  expect_equal(years, c(2024L, 2020L))

  nit <- retrieve_nitrogen(overview_row("Nitrogen", year = 2025L), "t")
  expect_equal(nit$message, "Failed: Nitrogen - no raster returned for year 2025")
  expect_equal(nit$file_type, "tif")
})

test_that("NDVI rasters: a single month, a range, and an invalid period", {
  calls <- character()
  local_mocked_bindings(
    gm_ndvi_month = function(roi, year, month, ...) { calls <<- c(calls, paste("month", year, month)); NULL },
    gm_ndvi_stack = function(roi, trange, ...) {
      calls <<- c(calls, paste("stack", as.integer(format(trange[1], "%Y")), as.integer(format(trange[1], "%m")), as.integer(format(trange[2], "%Y")), as.integer(format(trange[2], "%m"))))
      NULL
    },
    .package = "rNDC"
  )
  month <- retrieve_ndvi_rasters(overview_row("NDVI", from = date("2025-06-01"), to = date("2025-06-30")), 4)
  expect_equal(month$message, "NDVI 2025-06: No data available for this month")
  expect_equal(month$status, "No data available for this month")  # the summary shows the reason

  range <- retrieve_ndvi_rasters(overview_row("NDVI", from = date("2025-06-01"), to = date("2025-08-31")), 4)
  expect_equal(range$message, "NDVI - No data available for this period (2025-06-01 to 2025-08-31)")

  invalid <- retrieve_ndvi_rasters(overview_row("NDVI"), 4)
  expect_equal(invalid$message, "Failed: NDVI - invalid date range for row 4")
  expect_equal(invalid$status, "failed")

  expect_equal(calls, c("month 2025 6", "stack 2025 6 2025 8"))
})
