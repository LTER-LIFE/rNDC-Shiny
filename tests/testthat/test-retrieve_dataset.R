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

# what rNDC gives for the AHN and the soil map
height_rasters <- function() {
  dtm <- terra::rast(nrows = 2, ncols = 2, vals = 1:4); names(dtm) <- "AHN_DTM_2019"
  ohn <- terra::rast(nrows = 4, ncols = 4, vals = 1:16); names(ohn) <- "AHN_OHN_2022"
  list(rasters = list(AHN_DTM_2019 = dtm, AHN_OHN_2022 = ohn), stack = NULL,
       metadata = tibble::tibble(layer = c("dtm", "ohn"), year = c("2019", "2022")))
}

soil_legend <- function() tibble::tibble(code = c(1L, 14L), label = c("Veengronden", "Leemgronden"), color = c("#1920A7", "#FE021E"))

test_that("AHN statistics are a table with the polygon, from the NatureDataCube", {
  seen <- NULL
  local_mocked_bindings(ndc_ahn_stats = function(roi, token, ...) {
    seen <<- token
    tibble::tibble(layer = c("dtm", "ohn"), year = c("2019", "2022"), area = 10, min = 0, max = 1, mean = 0.5, sd = 0.1, n_cells = 4)
  }, .package = "rNDC")
  out <- retrieve_ahn(overview_row("AHN", "Statistics", name = "Area A"), "ndc-token")
  expect_equal(seen, "ndc-token")                       # no AgroDataCube token
  expect_equal(out$ext, "csv")
  expect_equal(out$data$polygon, c("Area A", "Area A"))
  expect_equal(out$data$layer, c("dtm", "ohn"))
  expect_equal(out$ok_msg, "Retrieved: AHN statistics")
  expect_null(out$extras)
})

test_that("AHN rasters are one file for each layer, the first being the data of the row", {
  local_mocked_bindings(ndc_ahn_raster = function(roi, token, ...) height_rasters(), .package = "rNDC")
  out <- retrieve_ahn(overview_row("AHN", "Geodata"), "t")
  expect_equal(c(out$name, out$ext, out$suffix), c("AHN_dtm", "tif", "dtm"))
  expect_equal(out$ok_msg, "Retrieved: AHN terrain height raster 2019")
  expect_length(out$extras, 1)
  expect_equal(c(out$extras[[1]]$name, out$extras[[1]]$ext, out$extras[[1]]$suffix), c("AHN_ohn", "tif", "ohn"))
  expect_equal(out$extras[[1]]$ok_msg, "Retrieved: AHN object height raster 2022")
  expect_equal(terra::ncell(out$extras[[1]]$data), 16)
})

test_that("the soil map is a raster, with its legend as a table", {
  local_mocked_bindings(ndc_soil_raster = function(roi, token, ...) {
    r <- terra::rast(nrows = 2, ncols = 2, vals = c(1, 14, 14, 16))
    terra::coltab(r) <- data.frame(value = c(1, 14, 16), col = c("#1920A7", "#FE021E", "#000000"))
    list(rasters = list(SOIL_2025 = r), stack = r, metadata = tibble::tibble(year = "2025"), legend = soil_legend())
  }, .package = "rNDC")
  out <- retrieve_soil_map(overview_row("Soil map"), "t")
  expect_equal(c(out$name, out$ext), c("Soil map", "tif"))
  expect_false(any(terra::has.colors(out$data)))
  expect_equal(out$ok_msg, "Retrieved: Soil map raster 2025")
  expect_null(out$suffix)
  expect_equal(c(out$extras[[1]]$ext, out$extras[[1]]$suffix), c("csv", "legend"))
  expect_equal(out$extras[[1]]$data$label, c("Veengronden", "Leemgronden"))

  local_mocked_bindings(ndc_soil_raster = function(roi, token, ...) {
    r <- terra::rast(nrows = 2, ncols = 2, vals = 1:4)
    list(rasters = list(SOIL_2025 = r), stack = r, metadata = tibble::tibble(year = "2025"), legend = NULL)
  }, .package = "rNDC")
  expect_length(retrieve_soil_map(overview_row("Soil map"), "t")$extras, 0)
})

test_that("the files of a dataset with several are written next to each other, and the data are returned for each", {
  local_mocked_bindings(ndc_ahn_raster = function(roi, token, ..., save_params = NULL) {
    if (!is.null(save_params)) writeLines("{}", save_params)
    height_rasters()
  }, ndc_soil_raster = function(roi, token, ..., save_params = NULL) {
    r <- terra::rast(nrows = 2, ncols = 2, vals = c(1, 14, 14, 16))
    list(rasters = list(SOIL_2025 = r), stack = r, metadata = tibble::tibble(year = "2025"), legend = soil_legend())
  }, .package = "rNDC")
  dir <- withr::local_tempdir()
  ahn <- retrieve_row(overview_row("AHN", "Geodata", name = "Area A"), 1, dir, "t", "")       # no AgroDataCube token needed
  soil <- retrieve_row(overview_row("Soil map", name = "Area A"), 2, dir, "t", "")

  expect_setequal(list.files(dir), c("area_a.gpkg", "ahn_geodata_area_a_dtm.tif", "ahn_geodata_area_a_ohn.tif",
                                     "soil_map_geodata_area_a.tif", "soil_map_geodata_area_a_legend.csv", "parameters"))
  expect_equal(vapply(ahn$manifest, function(m) m$file_path, ""), c("area_a.gpkg", "ahn_geodata_area_a_dtm.tif", "ahn_geodata_area_a_ohn.tif"))
  expect_equal(vapply(soil$manifest, function(m) m$file_type, ""), c("tif", "csv"))
  expect_equal(vapply(ahn$manifest, function(m) m$status, ""), c("ok", "ok", "ok"))
  expect_equal(ahn$manifest[[2]]$parameters, "parameters/ahn_geodata_area_a_dtm.json")   # the call is recorded once
  expect_true(is.na(ahn$manifest[[3]]$parameters))
  expect_equal(terra::ncell(terra::rast(file.path(dir, "ahn_geodata_area_a_ohn.tif"))), 16)
  expect_equal(utils::read.csv(file.path(dir, "soil_map_geodata_area_a_legend.csv"))$code, c(1, 14))

  # without files: the data of every file is returned, by name
  none <- retrieve_row(overview_row("AHN", "Geodata"), 3, workdir = NULL, ndc_token = "t", adc_token = "")
  expect_equal(none$name, "AHN_dtm_3")
  expect_named(none$more, "AHN_ohn_3")
  expect_length(none$manifest, 2)
  packaged <- retrieve_and_package(rbind(overview_row("AHN", "Geodata"), overview_row("Soil map")), save_files = FALSE,
                                   ndc_token = "t", adc_token = "")
  expect_named(packaged$datasets, c("AHN_dtm_1", "AHN_ohn_1", "Soil map_2", "Soil_map_legend_2"))
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
    ndc_ndvi_month = function(roi, year, month, ...) { calls <<- c(calls, paste("month", year, month)); NULL },
    ndc_ndvi_stack = function(roi, trange, ...) {
      calls <<- c(calls, paste("stack", as.integer(format(trange[1], "%Y")), as.integer(format(trange[1], "%m")), as.integer(format(trange[2], "%Y")), as.integer(format(trange[2], "%m"))))
      NULL
    },
    .package = "rNDC"
  )
  month <- retrieve_ndvi_rasters(overview_row("NDVI", from = date("2025-06-01"), to = date("2025-06-30")), 4, "t")
  expect_equal(month$message, "NDVI 2025-06: No data available for this month")
  expect_equal(month$status, "No data available for this month")  # the summary shows the reason

  range <- retrieve_ndvi_rasters(overview_row("NDVI", from = date("2025-06-01"), to = date("2025-08-31")), 4, "t")
  expect_equal(range$message, "NDVI - No data available for this period (2025-06-01 to 2025-08-31)")

  invalid <- retrieve_ndvi_rasters(overview_row("NDVI"), 4, "t")
  expect_equal(invalid$message, "Failed: NDVI - invalid date range for row 4")
  expect_equal(invalid$status, "failed")

  expect_equal(calls, c("month 2025 6", "stack 2025 6 2025 8"))
})

test_that("the data of the rows is not kept when it is only written", {
  local_mocked_bindings(ndc_ahn_raster = function(roi, token, ..., save_params = NULL) height_rasters(), .package = "rNDC")
  ov <- overview_row("AHN", "Geodata")
  written <- retrieve_and_package(ov, zipfile = NULL, save_files = TRUE, workdir = withr::local_tempdir(), ndc_token = "t",
                                  return_data = FALSE, parameters = FALSE)
  expect_length(written$datasets, 0)
  expect_equal(written$summary$status, c("ok", "ok", "ok"))
  expect_equal(sum(written$summary$file_type == "tif", na.rm = TRUE), 2)       # both rasters were written all the same

  kept <- retrieve_and_package(ov, save_files = TRUE, workdir = withr::local_tempdir(), ndc_token = "t", return_data = TRUE,
                               parameters = FALSE)
  expect_named(kept$datasets, c("AHN_dtm_1", "AHN_ohn_1"))
  not_returned <- retrieve_and_package(ov, save_files = FALSE, ndc_token = "t", return_data = FALSE, parameters = FALSE)
  expect_length(not_returned$datasets, 0)
})
