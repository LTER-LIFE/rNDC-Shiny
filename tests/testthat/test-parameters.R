# The parameters of the retrievals in the download (JSON files, R/retrieval.R): to repeat a retrieval in R with
# rNDC::ndc_repeat().

soil_stub <- function() {
  webmockr::stub_request("get", uri_regex = adc_re("soiltypes")) |>
    webmockr::to_return(body = json_body(geojson_features(1)), headers = json_header)
}

test_that("a download has the parameters of every dataset, named after its data, and they repeat the retrieval", {
  local_webmock()
  soil_stub()
  dir <- withr::local_tempdir()
  res <- retrieve_row(overview_row("Soil map", name = "Area A"), 1, dir, "t", "t")

  expect_equal(list.files(file.path(dir, "parameters")), "soil_map_geodata_area_a.json")
  expect_equal(res$manifest[[2]]$parameters, "parameters/soil_map_geodata_area_a.json")
  expect_true(is.na(res$manifest[[1]]$parameters))                          # the reference polygon
  expect_length(list.files(dir, pattern = "^\\.params", all.files = TRUE), 0) # nothing left of the work

  file <- file.path(dir, "parameters", "soil_map_geodata_area_a.json")
  recorded <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_equal(recorded$schema, "rNDC-adc_get_all/1")
  expect_equal(recorded$option, "Soiltypes")
  expect_false(grepl("token", paste(readLines(file), collapse = ""), ignore.case = TRUE))

  # what the file says is what was sent: repeating it asks the same
  before <- length(request_uris())
  again <- rNDC::ndc_repeat(file, token = "t")
  expect_equal(length(again$features), 1)
  expect_equal(tail(request_uris(), 1), request_uris()[before])
})

test_that("Weather records the station and the data as two files, and the summary points to the data", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("meteostations")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection",
                                              features = list(list(type = "Feature", geometry = list(type = "Point", coordinates = c(5, 52)),
                                                                   properties = list(meteostationid = 260))))),
                        headers = json_header)
  webmockr::stub_request("get", uri_regex = adc_re("meteodata")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection",
                                              features = list(list(type = "Feature", geometry = list(type = "Point", coordinates = c(5, 52)),
                                                                   properties = list(datum = "2024-05-01", mean_temperature = 10))))),
                        headers = json_header)
  dir <- withr::local_tempdir()
  res <- retrieve_row(overview_row("Weather", "Statistics", from = as.Date("2024-05-01"), to = as.Date("2024-05-01")), 1, dir, "t", "t")

  expect_setequal(list.files(file.path(dir, "parameters")), c("weather_statistics_own_polygon.json",
                                                              "weather_statistics_own_polygon_station.json"))
  expect_equal(res$manifest[[2]]$parameters, "parameters/weather_statistics_own_polygon.json")
  station <- jsonlite::fromJSON(file.path(dir, "parameters", "weather_statistics_own_polygon_station.json"), simplifyVector = FALSE)
  data <- jsonlite::fromJSON(file.path(dir, "parameters", "weather_statistics_own_polygon.json"), simplifyVector = FALSE)
  expect_equal(station$schema, "rNDC-get_closest_meteostation/1")
  expect_equal(station$polygon_wkt$type, "Polygon")
  expect_equal(data$schema, "rNDC-get_meteo_for_date/1")
  expect_equal(c(data$stationid, data$date), c("260", "2024-05-01"))
  expect_equal(rNDC::ndc_repeat(file.path(dir, "parameters", "weather_statistics_own_polygon.json"), token = "t")$mean_temperature, 10)
})

test_that("the parameters of a dataset that was not retrieved are not kept", {
  local_stac_api()
  local_mocked_bindings(get_landuse_raster = function(..., save_params = NULL) {
    if (!is.null(save_params)) writeLines("{}", save_params)   # (recorded before the problem)
    NULL
  }, .package = "rNDC")
  dir <- withr::local_tempdir()
  res <- retrieve_row(overview_row("Land Use", year = 2024L), 1, dir, "t", "t")
  expect_null(res$data)
  expect_false(dir.exists(file.path(dir, "parameters")))
  expect_true(is.na(res$manifest[[2]]$parameters))
  expect_length(list.files(dir, pattern = "^\\.params", all.files = TRUE), 0)
})

test_that("without the option there are no parameters, and the functions are not asked to record any", {
  local_webmock()
  soil_stub()
  seen <- list()
  local_mocked_bindings(adc_get_all = function(..., save_params = NULL) {
    seen <<- c(seen, list(is.null(save_params)))
    list(type = "FeatureCollection", features = geojson_features(1)$features)
  }, .package = "rNDC")
  dir <- withr::local_tempdir()
  res <- retrieve_row(overview_row("Soil map"), 1, dir, "t", "t", parameters = FALSE)
  expect_false(dir.exists(file.path(dir, "parameters")))
  expect_true(seen[[1]])   # (the argument was NULL)
  expect_true(all(is.na(vapply(res$manifest, function(m) m$parameters, ""))))
})

test_that("when nothing is written the parameters are returned as texts, and nothing is left behind", {
  local_webmock()
  soil_stub()
  before <- list.files(tempdir(), "^ndc_params_")
  res <- retrieve_and_package(overview_row("Soil map"), save_files = FALSE, ndc_token = "t", adc_token = "t")
  expect_named(res$parameters, "Soil map_1")
  expect_named(res$parameters[["Soil map_1"]], "data")
  expect_equal(jsonlite::fromJSON(res$parameters[["Soil map_1"]]$data)$option, "Soiltypes")
  expect_identical(list.files(tempdir(), "^ndc_params_"), before)

  none <- retrieve_and_package(overview_row("Soil map"), save_files = FALSE, ndc_token = "t", adc_token = "t",
                               parameters = FALSE)
  expect_length(none$parameters, 0)
})

test_that("the download has a checkbox for the parameters, which the retrieval follows", {
  local_clean_cache()
  local_stac_api()
  recorded <- list()
  local_mocked_bindings(get_landuse_raster = function(..., save_params = NULL) {
    recorded[[length(recorded) + 1]] <<- is.null(save_params)
    if (!is.null(save_params)) writeLines("{}", save_params)
    list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4))
  }, .package = "rNDC")

  with_overview(overview_row("Land Use", year = 2024L), {
    session$flushReact()
    expect_match(as.character(output$download_ui$html), "include_parameters", fixed = TRUE)   # on by default
    expect_match(as.character(output$download_ui$html), "checked", fixed = TRUE)

    retrieve_and_save(zipfile = zip, save_files = TRUE)
    expect_true("parameters/land_use_geodata_own_polygon.json" %in% utils::unzip(zip, list = TRUE)$Name)

    session$setInputs(include_parameters = FALSE)
    zip2 <- tempfile(fileext = ".zip")
    retrieve_and_save(zipfile = zip2, save_files = TRUE)
    expect_false(any(grepl("^parameters/", utils::unzip(zip2, list = TRUE)$Name)))
  })
  expect_equal(unlist(recorded), c(FALSE, TRUE))   # was it asked to record: yes, then no
})

test_that("the background job is told whether to record the parameters", {
  local_stac_api()
  fake <- fake_manager(run = TRUE)
  local_jobs(fake$manager)
  local_mocked_bindings(get_landuse_raster = function(..., save_params = NULL) {
    if (!is.null(save_params)) writeLines("{}", save_params)
    list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4))
  }, .package = "rNDC")

  with_server({
    overview(overview_row("Land Use", year = 2024L))
    session$setInputs(include_parameters = FALSE, check_and_download = 1)
    settle(session, function() !retrieving())
    expect_false(fake$rec$jobs[[1]]$args$parameters)
  })
})
