# The functions behind retrieve_and_save() (R/retrieval.R), tested without a Shiny session.

date <- as.Date

test_that("outcomes describe either data or why there is none", {
  out <- outcome_data(1:3, "Thing", "csv", ok_msg = "ok", fail_msg = "fail")
  expect_equal(out$name, "Thing")
  expect_equal(out$ext, "csv")
  expect_true(is.function(out$writer))

  none <- outcome_none("msg", "tif")
  expect_null(none$data)
  expect_equal(none$status, "failed")
  expect_equal(outcome_none("msg", NA_character_, status = "skipped")$status, "skipped")
})

test_that("vector datasets are tables in the Statistics view and GeoPackages otherwise", {
  expect_equal(vector_ext(overview_row("AHN", "Statistics")), "csv")
  expect_equal(vector_ext(overview_row("AHN", "statistics")), "csv")
  expect_equal(vector_ext(overview_row("Soil map", "Geodata")), "gpkg")
})

test_that("format_date_label describes a year, a date or a period", {
  expect_equal(format_date_label(2024L, date(NA), date(NA)), "2024")
  expect_equal(format_date_label(NA, date("2024-05-01"), date("2024-05-01")), "2024-05-01")
  expect_equal(format_date_label(NA, date("2024-05-01"), date("2024-05-31")), "2024-05-01 - 2024-05-31")
  expect_equal(format_date_label(NA, date("2024-05-01"), date(NA)), "2024-05-01")
  expect_equal(format_date_label(NA, date(NA), date("2024-05-31")), "2024-05-31")
  expect_equal(format_date_label(NA, date(NA), date(NA)), "")
})

test_that("manifest_row has the columns of the download summary", {
  expect_named(manifest_row("d", "v", "p", "2024", "csv", "f.csv", "ok"),
               c("dataset", "view", "polygon", "date", "file_type", "file_path", "status", "note",
                 "reference", "license"))
})

test_that("any_data_produced ignores the polygon helper file and failures", {
  rows <- function(...) dplyr::bind_rows(...)
  poly <- manifest_row("Area", "Reference polygon", "p", "", "gpkg", "p.gpkg", "ok", note = "selected polygon geometry")
  bad <- manifest_row("AHN", "Statistics", "p", "", NA, NA, "failed")
  good <- manifest_row("AHN", "Statistics", "p", "", "csv", "a.csv", "ok")
  expect_false(any_data_produced(tibble::tibble()))
  expect_false(any_data_produced(rows(poly)))
  expect_false(any_data_produced(rows(poly, bad)))
  expect_true(any_data_produced(rows(poly, bad, good)))
})

test_that("write_sf_safe writes the format of the extension and refuses what does not fit", {
  dir <- withr::local_tempdir()
  pts <- sf::st_sf(a = 1:2, geometry = sf::st_sfc(sf::st_point(c(5, 52)), sf::st_point(c(6, 53)), crs = 4326))
  r <- terra::rast(nrows = 2, ncols = 2, vals = 1:4)

  expect_true(write_sf_safe(pts, file.path(dir, "a.csv")))
  expect_equal(names(utils::read.csv(file.path(dir, "a.csv"))), "a")  # no geometry column
  expect_true(write_sf_safe(pts, file.path(dir, "a.gpkg")))
  expect_equal(nrow(sf::st_read(file.path(dir, "a.gpkg"), quiet = TRUE)), 2)
  expect_true(write_sf_safe(r, file.path(dir, "a.tif")))
  expect_s4_class(terra::rast(file.path(dir, "a.tif")), "SpatRaster")
  expect_true(write_sf_safe(data.frame(x = 1), file.path(dir, "b.csv")))

  expect_false(write_sf_safe(NULL, file.path(dir, "n.csv")))
  expect_false(write_sf_safe(data.frame(), file.path(dir, "empty.csv")))
  expect_false(write_sf_safe(r, file.path(dir, "r.gpkg")))   # a raster is not a vector layer
  expect_false(write_sf_safe(pts, file.path(dir, "p.tif")))  # nor the reverse
  expect_false(write_sf_safe(pts, file.path(dir, "p.xyz")))
  expect_false(file.exists(file.path(dir, "p.xyz")))
})

test_that("zip_export zips the export folder without changing the working directory", {
  dir <- withr::local_tempdir()  # the export folder is flat
  writeLines("a", file.path(dir, "a.txt"))
  writeLines("b", file.path(dir, "b.csv"))
  zip <- withr::local_tempfile(fileext = ".zip")
  wd <- getwd()
  zip_export(dir, zip)
  expect_identical(getwd(), wd)
  expect_setequal(utils::unzip(zip, list = TRUE)$Name, c("a.txt", "b.csv"))
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
    get_closest_meteostation = function(...) list(closest_id = station_id),
    get_meteo_for_date = function(...) NULL,
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

test_that("weather: one date uses get_meteo_for_date and a period the long-period function", {
  calls <- character()
  meteo <- sf::st_sf(day = 1, geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326))
  local_mocked_bindings(
    get_closest_meteostation = function(...) list(closest_id = "260"),
    get_meteo_for_date = function(id, date, token) { calls <<- c(calls, paste("date", id, format(date), token)); meteo },
    get_meteo_for_long_period = function(meteostation, fromdate, todate, token, by_days, sleep_sec) {
      calls <<- c(calls, paste("period", meteostation, format(fromdate), format(todate), by_days))
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
    get_landuse_raster = function(aoi, year, ...) { years <<- c(years, year); list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4)) },
    get_nitrogen_raster = function(aoi, year, layers, ...) list(stack = NULL),
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
    download_avg_ndvi_month = function(poly, year, month, ...) { calls <<- c(calls, paste("month", year, month)); NULL },
    download_avg_ndvi_stack = function(poly, start_year, start_month, end_year, end_month, ...) {
      calls <<- c(calls, paste("stack", start_year, start_month, end_year, end_month))
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

test_that("retrieve_row returns the data without writing anything when there is no work folder", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("soiltypes")) |>
    webmockr::to_return(body = json_body(geojson_features(1:3, "soiltype")), headers = json_header)

  res <- retrieve_row(overview_row("Soil map"), 7, workdir = NULL, ndc_token = "t", adc_token = "t")
  expect_equal(res$name, "Soil map_7")
  expect_equal(nrow(res$data), 3)
  expect_equal(res$messages, "Retrieved: Soil map")
  expect_length(res$manifest, 1)
  expect_equal(res$manifest[[1]]$status, "ok")
  expect_true(is.na(res$manifest[[1]]$file_path))
})

test_that("retrieve_row writes the data, and the reference polygon once per polygon", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("soiltypes")) |>
    webmockr::to_return(body = json_body(geojson_features(1:3, "soiltype")), headers = json_header)
  webmockr::stub_request("get", uri_regex = adc_re("ahn")) |>
    webmockr::to_return(body = json_body(geojson_features(1, "ahn")), headers = json_header)
  dir <- withr::local_tempdir()

  first <- retrieve_row(overview_row("Soil map", name = "Area A"), 1, dir, "t", "t")
  second <- retrieve_row(overview_row("AHN", "Statistics", name = "Area A"), 2, dir, "t", "t")

  expect_equal(vapply(first$manifest, function(m) m$dataset, ""), c("Area", "Soil map"))
  expect_equal(vapply(second$manifest, function(m) m$dataset, ""), "AHN")  # polygon already exported
  expect_setequal(list.files(dir), c("area_a.gpkg", "soil_map_geodata_area_a.gpkg", "ahn_statistics_area_a.csv"))
  expect_equal(first$manifest[[1]]$note, "selected polygon geometry")
  expect_equal(first$manifest[[2]]$file_path, "soil_map_geodata_area_a.gpkg")
})

test_that("retrieve_row reports an error of the retrieval or of the writing for that row only", {
  local_mocked_bindings(get_landuse_raster = function(...) stop("boom", call. = FALSE), .package = "rNDC")
  res <- retrieve_row(overview_row("Land Use", year = 2024L), 3, workdir = NULL, ndc_token = "t", adc_token = "t")
  expect_null(res$data)
  expect_equal(res$messages, "Failed: Land Use - boom")
  expect_equal(res$manifest[[1]]$status, "failed")
  expect_equal(res$manifest[[1]]$note, "boom")

  # A writer that cannot write: the data is still returned, with the message for it
  local_mocked_bindings(
    get_landuse_raster = function(...) list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4)),
    .package = "rNDC"
  )
  local_mocked_bindings(write_sf_safe = function(...) FALSE)
  res <- retrieve_row(overview_row("Land Use", year = 2024L), 3, withr::local_tempdir(), "t", "t")
  expect_s4_class(res$data, "SpatRaster")
  expect_equal(res$messages, "Failed: Land Use - could not write output")
  expect_equal(res$manifest[[2]]$status, "failed")
})

test_that("a writer that throws is reported as a failure of that dataset, keeping the data", {
  local_mocked_bindings(get_landuse_raster = function(...) list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4)),
                        .package = "rNDC")
  local_mocked_bindings(write_sf_safe = function(...) stop("disk full", call. = FALSE))
  res <- retrieve_row(overview_row("Land Use", year = 2024L), 1, withr::local_tempdir(), "t", "t")
  expect_s4_class(res$data, "SpatRaster")  # as before the refactoring: the data survive a failing write
  expect_equal(res$messages, "Failed: Land Use - disk full")
})

test_that("unique_export_file keeps the plain name and otherwise appends the period, or a number", {
  dir <- withr::local_tempdir()
  plain <- unique_export_file(dir, "ahn_statistics_p", "csv", "2024")
  expect_equal(basename(plain), "ahn_statistics_p.csv")

  file.create(plain)
  dated <- unique_export_file(dir, "ahn_statistics_p", "csv", "2024")
  expect_equal(basename(dated), "ahn_statistics_p_2024.csv")
  expect_equal(basename(unique_export_file(dir, "ahn_statistics_p", "csv", "2024-05-01 - 2024-05-31")),
               "ahn_statistics_p_2024-05-01_to_2024-05-31.csv")

  file.create(dated)
  expect_equal(basename(unique_export_file(dir, "ahn_statistics_p", "csv", "2024")), "ahn_statistics_p_2.csv")
  expect_equal(basename(unique_export_file(dir, "ahn_statistics_p", "csv", "")), "ahn_statistics_p_2.csv")

  # Another extension or base name is not a collision
  expect_equal(basename(unique_export_file(dir, "ahn_statistics_p", "gpkg", "2024")), "ahn_statistics_p.gpkg")
})

test_that("rows that differ only in year do not overwrite each other's file", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("fields")) |>
    webmockr::to_return(body = json_body(geojson_features(1:2)), headers = json_header) |>
    webmockr::to_return(body = json_body(geojson_features(1:5)), headers = json_header)
  dir <- withr::local_tempdir()

  y2024 <- retrieve_row(overview_row("Agricultural fields", year = 2024L), 1, dir, "t", "t")
  y2023 <- retrieve_row(overview_row("Agricultural fields", year = 2023L), 2, dir, "t", "t")

  files <- list.files(dir, pattern = "^agricultural")
  expect_setequal(files, c("agricultural_fields_geodata_own_polygon.gpkg",
                           "agricultural_fields_geodata_own_polygon_2023.gpkg"))
  # The summary says which file belongs to which year, and each file holds its own data
  expect_equal(y2024$manifest[[2]]$file_path, "agricultural_fields_geodata_own_polygon.gpkg")
  expect_equal(y2023$manifest[[1]]$file_path, "agricultural_fields_geodata_own_polygon_2023.gpkg")
  expect_equal(nrow(sf::st_read(file.path(dir, y2024$manifest[[2]]$file_path), quiet = TRUE)), 2)
  expect_equal(nrow(sf::st_read(file.path(dir, y2023$manifest[[1]]$file_path), quiet = TRUE)), 5)
})

other_polygon <- function() {
  sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = 6.1, ymin = 52.1, xmax = 6.2, ymax = 52.2), crs = sf::st_crs(4326))))
}

test_that("the reference polygon is exported once for the same polygon, and again for another with the same name", {
  dir <- withr::local_tempdir()
  same <- overview_row("AHN", name = "Area A")
  other <- same
  other$polygon_sf <- list(other_polygon())

  first <- export_reference_polygon(same, dir, "area_a", "2024")
  again <- export_reference_polygon(same, dir, "area_a", "2023")  # the same polygon: nothing new
  second <- export_reference_polygon(other, dir, "area_a", "2024")  # another polygon with the same name
  third <- export_reference_polygon(other, dir, "area_a", "2025")

  expect_equal(first$file_path, "area_a.gpkg")
  expect_null(again)
  expect_equal(second$file_path, "area_a_2.gpkg")
  expect_null(third)
  expect_setequal(list.files(dir), c("area_a.gpkg", "area_a_2.gpkg"))
  expect_true(all(c(first$status, second$status) == "ok"))
  # each file holds its own polygon
  expect_equal(sf::st_bbox(sf::st_read(file.path(dir, "area_a_2.gpkg"), quiet = TRUE))[["xmin"]], 6.1)
  expect_equal(sf::st_bbox(sf::st_read(file.path(dir, "area_a.gpkg"), quiet = TRUE))[["xmin"]], 5.75)
})

test_that("a polygon file with the name of another is not mistaken for it", {
  dir <- withr::local_tempdir()
  geom <- sf::st_sf(polygon = "a", geometry = sf::st_geometry(selected_polygon()))
  expect_equal(basename(reference_polygon_file(dir, "p", geom)), "p.gpkg")  # nothing there yet

  sf::st_write(geom, file.path(dir, "p.gpkg"), quiet = TRUE)
  expect_null(reference_polygon_file(dir, "p", geom))
  expect_true(same_polygon(file.path(dir, "p.gpkg"), geom))
  # the same polygon in another CRS is still the same polygon
  expect_true(same_polygon(file.path(dir, "p.gpkg"), sf::st_transform(geom, 28992)))

  other <- sf::st_sf(polygon = "b", geometry = sf::st_geometry(other_polygon()))
  expect_false(same_polygon(file.path(dir, "p.gpkg"), other))
  expect_equal(basename(reference_polygon_file(dir, "p", other)), "p_2.gpkg")

  writeLines("not a geopackage", file.path(dir, "broken.gpkg"))
  expect_false(same_polygon(file.path(dir, "broken.gpkg"), geom))
})
