# retrieve_and_save(): the retrieval of the datasets in the overview, with the APIs stubbed or mocked.

export_dirs <- function() list.files(tempdir(), "^ndc_export_")

# Run `code` inside the server with `rows` in the overview; `code` can use retrieve_and_save() and `zip`.
with_overview <- function(rows, code) {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t", .local_envir = parent.frame())
  code <- substitute(code)
  shiny::testServer(app_server, {
    overview(dplyr::bind_rows(rows))
    eval(code, list2env(list(zip = tempfile(fileext = ".zip")), parent = environment()))
  })
}

test_that("Agricultural fields are retrieved page by page, zipped, and the export folder is removed", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("fields")) |>
    webmockr::to_return(body = json_body(geojson_features(1:1000)), headers = json_header) |>
    webmockr::to_return(body = json_body(geojson_features(1001:1003)), headers = json_header)
  wd <- getwd()
  before <- export_dirs()

  with_overview(overview_row("Agricultural fields", year = 2024L), {
    res <- retrieve_and_save(zipfile = zip, save_files = TRUE)

    expect_true(res$produced_any)
    expect_true(file.exists(zip))
    expect_equal(nrow(res$datasets$`Agricultural fields_1`), 1003)
    expect_equal(res$summary$status, c("ok", "ok"))
    expect_equal(res$messages, "Retrieved: Agricultural fields")

    dir <- withr::local_tempdir()
    utils::unzip(zip, exdir = dir)
    expect_setequal(list.files(dir), c("agricultural_fields_geodata_own_polygon.gpkg",
                                       "download_summary.csv", "own_polygon.gpkg"))
    expect_equal(nrow(sf::st_read(file.path(dir, "agricultural_fields_geodata_own_polygon.gpkg"), quiet = TRUE)), 1003)
  })
  expect_identical(getwd(), wd)
  expect_identical(export_dirs(), before)
  expect_length(request_uris(), 2)
})

test_that("retrieve_and_save() can return the data without writing files", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("soiltypes")) |>
    webmockr::to_return(body = json_body(geojson_features(1:3, "soiltype")), headers = json_header)
  before <- export_dirs()

  with_overview(overview_row("Soil map"), {
    res <- retrieve_and_save(save_files = FALSE)
    expect_equal(nrow(res$datasets$`Soil map_1`), 3)
    expect_true(res$produced_any)
    expect_null(res$out_dir)
  })
  expect_identical(export_dirs(), before)
})

test_that("a failing API call is reported for that dataset and nothing is zipped", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("ahn")) |>
    webmockr::to_return(body = json_body(list(status = "Unknown token")), status = 403, headers = json_header)

  with_overview(overview_row("AHN", view = "Statistics"), {
    res <- retrieve_and_save(zipfile = zip, save_files = TRUE)
    expect_false(res$produced_any)
    expect_false(file.exists(zip))
    expect_match(res$messages, "^Failed: AHN - .*403.*Unknown token")
    expect_equal(res$summary$status[res$summary$dataset == "AHN"], "failed")
  })
})

station <- function(properties) {
  list(type = "Feature", geometry = list(type = "Point", coordinates = c(5.8, 52.0)), properties = properties)
}

test_that("weather: no station id gives a clear message", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("meteostations")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection", features = list(station(list(name = "x"))))),
                        headers = json_header)

  with_overview(overview_row("Weather", "Statistics", from = as.Date("2024-05-01"), to = as.Date("2024-05-01")), {
    res <- retrieve_and_save(zipfile = zip, save_files = TRUE)
    expect_false(res$produced_any)
    expect_equal(res$messages, "Failed: Weather - no nearby station found")
  })
})

test_that("weather for a date is written as csv", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("meteostations")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection",
                                              features = list(station(list(meteostationid = 260))))),
                        headers = json_header)
  webmockr::stub_request("get", uri_regex = adc_re("meteodata")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection", features = list(
      # observations have no geometry: the API sends "geometry": null
      list(type = "Feature", geometry = NULL, properties = list(datum = "2024-05-01", mean_temperature = 12.3))))),
      headers = json_header)

  with_overview(overview_row("Weather", "Statistics", from = as.Date("2024-05-01"), to = as.Date("2024-05-01")), {
    res <- retrieve_and_save(zipfile = zip, save_files = TRUE)
    expect_true(res$produced_any)
    dir <- withr::local_tempdir()
    utils::unzip(zip, exdir = dir)
    csv <- utils::read.csv(file.path(dir, "weather_statistics_own_polygon.csv"))
    expect_equal(csv$mean_temperature, 12.3)
  })
  expect_match(request_uris()[grepl("meteodata", request_uris())], "stationid=260")
})

test_that("a single NDVI month is retrieved from the dates of its overview row, not from the inputs", {
  requested <- NULL
  local_mocked_bindings(
    download_avg_ndvi_month = function(poly, year, month, ...) {
      requested <<- c(year, month)
      terra::rast(nrows = 2, ncols = 2, vals = 1:4)
    },
    .package = "rNDC"
  )
  row <- overview_row("NDVI", from = as.Date("2025-06-01"), to = as.Date("2025-06-30"))

  with_overview(row, {
    session$setInputs(ndvi_mode = "range", ndvi_year = 1999, ndvi_month = 1)  # stale widgets
    res <- retrieve_and_save(zipfile = zip, save_files = TRUE)
    expect_true(res$produced_any)
  })
  expect_equal(requested, c(2025, 6))
})

test_that("an NDVI range is retrieved as a stack with NDVI_ layer names", {
  args <- NULL
  local_mocked_bindings(
    download_avg_ndvi_stack = function(poly, start_year, start_month, end_year, end_month, ...) {
      args <<- c(start_year, start_month, end_year, end_month)
      r <- terra::rast(nrows = 2, ncols = 2, nlyrs = 2, vals = 1:8)
      names(r) <- c("ndvi_mean_202506", "ndvi_mean_202507")
      r
    },
    .package = "rNDC"
  )
  row <- overview_row("NDVI", from = as.Date("2025-06-01"), to = as.Date("2025-07-31"))

  with_overview(row, {
    res <- retrieve_and_save(save_files = FALSE)
    expect_equal(names(res$datasets$NDVI_1), c("NDVI_202506", "NDVI_202507"))
  })
  expect_equal(args, c(2025, 6, 2025, 7))
})

test_that("rasters that cannot be retrieved are reported with the reason", {
  local_mocked_bindings(
    get_landuse_raster = function(...) stop("No Land Use raster items matched year 2024.", call. = FALSE),
    get_nitrogen_raster = function(...) list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4)),
    .package = "rNDC"
  )
  rows <- list(overview_row("Land Use", year = 2024L), overview_row("Nitrogen", year = 2024L))

  with_overview(rows, {
    res <- retrieve_and_save(zipfile = zip, save_files = TRUE)
    expect_true(res$produced_any)  # nitrogen worked
    expect_equal(res$messages, c("Failed: Land Use - No Land Use raster items matched year 2024.",
                                 "Retrieved: Nitrogen raster for 2024"))
    expect_equal(res$summary$status[res$summary$dataset == "Land Use"], "failed")
    expect_equal(res$summary$status[res$summary$dataset == "Nitrogen"], "ok")
  })
})

test_that("the messages panel shows the result per dataset, escaped", {
  local_mocked_bindings(
    get_landuse_raster = function(...) stop("bad <b>input</b>", call. = FALSE),
    .package = "rNDC"
  )
  with_overview(overview_row("Land Use", year = 2024L), {
    retrieve_and_save(save_files = FALSE)
    session$flushReact()
    html <- as.character(output$download_messages$html)
    expect_match(html, "Failed: Land Use - bad &lt;b&gt;input&lt;/b&gt;", fixed = TRUE)
  })
})

test_that("downloading shows the reason when no dataset could be retrieved", {
  local_mocked_bindings(
    get_landuse_raster = function(...) stop("No Land Use raster items matched year 2024.", call. = FALSE),
    .package = "rNDC"
  )
  with_overview(overview_row("Land Use", year = 2024L), {
    session$setInputs(check_and_download = 1)
    session$flushReact()
    html <- as.character(output$download_messages$html)
    expect_match(html, "No data is available within your selection", fixed = TRUE)
    expect_match(html, "Failed: Land Use - No Land Use raster items matched year 2024.", fixed = TRUE)
    expect_null(prepared_zip())
  })
})

test_that("the same dataset for two years gives two files in the download", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("fields")) |>
    webmockr::to_return(body = json_body(geojson_features(1:2)), headers = json_header) |>
    webmockr::to_return(body = json_body(geojson_features(1:3)), headers = json_header)
  rows <- list(overview_row("Agricultural fields", year = 2024L), overview_row("Agricultural fields", year = 2023L))

  with_overview(rows, {
    res <- retrieve_and_save(zipfile = zip, save_files = TRUE)
    dir <- withr::local_tempdir()
    utils::unzip(zip, exdir = dir)
    expect_setequal(list.files(dir), c("agricultural_fields_geodata_own_polygon.gpkg",
                                       "agricultural_fields_geodata_own_polygon_2023.gpkg",
                                       "download_summary.csv", "own_polygon.gpkg"))
    summary <- utils::read.csv(file.path(dir, "download_summary.csv"))
    expect_equal(summary$file_path[summary$date == 2023], "agricultural_fields_geodata_own_polygon_2023.gpkg")
    expect_equal(nrow(res$datasets$`Agricultural fields_1`), 2)
    expect_equal(nrow(res$datasets$`Agricultural fields_2`), 3)
  })
})

test_that("two different polygons with the same name both end up in the download", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("soiltypes")) |>
    webmockr::to_return(body = json_body(geojson_features(1:2, "soiltype")), headers = json_header)
  second <- overview_row("Soil map")
  second$polygon_sf <- list(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
    c(xmin = 6.1, ymin = 52.1, xmax = 6.2, ymax = 52.2), crs = sf::st_crs(4326)))))
  second$wkt <- sf::st_as_text(sf::st_geometry(second$polygon_sf[[1]]))

  with_overview(list(overview_row("Soil map"), second), {
    res <- retrieve_and_save(zipfile = zip, save_files = TRUE)
    dir <- withr::local_tempdir()
    utils::unzip(zip, exdir = dir)
    expect_setequal(list.files(dir), c("own_polygon.gpkg", "own_polygon_2.gpkg", "soil_map_geodata_own_polygon.gpkg",
                                       "soil_map_geodata_own_polygon_2.gpkg", "download_summary.csv"))
    summary <- utils::read.csv(file.path(dir, "download_summary.csv"))
    expect_equal(sum(summary$dataset == "Area"), 2)
    expect_false(anyDuplicated(summary$file_path[!is.na(summary$file_path)]) > 0)
  })
})

test_that("the export folder is returned only when it still exists", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("soiltypes")) |>
    webmockr::to_return(body = json_body(geojson_features(1:2, "soiltype")), headers = json_header)

  with_overview(overview_row("Soil map"), {
    zipped <- retrieve_and_save(zipfile = zip, save_files = TRUE)
    expect_null(zipped$out_dir)  # removed after zipping
    expect_equal(zipped$zipfile, zip)

    kept <- retrieve_and_save(save_files = TRUE)  # no zip: the folder is the result
    withr::defer(unlink(kept$out_dir, recursive = TRUE))
    expect_true(dir.exists(kept$out_dir))
    expect_true(file.exists(file.path(kept$out_dir, "download_summary.csv")))
  })
})

test_that("the progress follows what the rNDC functions report during a long retrieval", {
  rec <- new.env()
  rec$calls <- list()
  local_mocked_bindings(
    report_progress = function(detail = NULL, value = NULL) {
      rec$calls[[length(rec$calls) + 1]] <- list(detail = detail, value = value)
    }
  )
  meteo <- sf::st_sf(day = 1, geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326))
  local_mocked_bindings(
    get_closest_meteostation = function(...) list(closest_id = "260"),
    get_meteo_for_long_period = function(...) {
      # as the real function does: report before each request (see rNDC::ndc_with_progress())
      for (i in 1:4) getOption("rNDC.progress")(paste0("Downloading chunk ", i, " (", i, "/4)"), i, 4L)
      meteo
    },
    .package = "rNDC"
  )
  rows <- list(overview_row("Land Use", year = 2024L),
               overview_row("Weather", "Statistics", from = date_from <- as.Date("2024-01-01"), to = as.Date("2024-06-30")))
  local_mocked_bindings(get_landuse_raster = function(...) list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4)),
                        .package = "rNDC")

  with_overview(rows, {
    suppressMessages(res <- retrieve_and_save(save_files = FALSE))
    expect_true(res$produced_any)
  })

  values <- vapply(rec$calls, function(x) if (is.null(x$value)) NA_real_ else x$value, numeric(1))
  details <- vapply(rec$calls, function(x) if (is.null(x$detail)) NA_character_ else x$detail, character(1))
  # each row starts at its place of the bar, and ends where the next one starts
  expect_equal(values[details == "Land Use (Own polygon)" & !is.na(details)][1], 0)
  expect_equal(values[details == "Weather (Own polygon)" & !is.na(details)][1], 0.5)
  # the 3rd of 4 requests of the 2nd of 2 rows
  expect_true(any(abs(values - (1 + 2 / 4) / 2) < 1e-9, na.rm = TRUE))
  expect_true(any(grepl("^Weather \\(Own polygon\\): Downloading chunk 3 \\(3/4\\)$", details)))
  expect_equal(values[length(values)], 1)
  expect_false(is.unsorted(values[!is.na(values)]))  # the bar never goes back
})
