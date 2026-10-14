# The download of the overview: retrieve_and_save() with the APIs stubbed or mocked, the download buttons, and the retrieval in the
# background with a queue and a cancel button. (R/server_download.R)

export_dirs <- function() list.files(tempdir(), "^ndc_export_")

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
                                       "download_summary.csv", "own_polygon.gpkg", "CITATIONS.txt", "parameters"))
    # the parameters of the retrieval, to repeat it in R (the download has the same area and year as the file says)
    expect_equal(list.files(file.path(dir, "parameters")), "agricultural_fields_geodata_own_polygon.json")
    recorded <- jsonlite::fromJSON(file.path(dir, "parameters", "agricultural_fields_geodata_own_polygon.json"),
                                   simplifyVector = FALSE)
    expect_equal(recorded$schema, "rNDC-adc_get_all/1")
    expect_equal(recorded$option, "Fields")
    expect_match(recorded$params$geometry, "^POLYGON")
    expect_false(grepl("token", paste(readLines(file.path(dir, "parameters", "agricultural_fields_geodata_own_polygon.json")),
                                      collapse = ""), ignore.case = TRUE))
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
    gm_ndvi_month = function(roi, year, month, ...) {
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
    gm_ndvi_stack = function(roi, trange, ...) {
      args <<- c(as.integer(format(trange[1], "%Y")), as.integer(format(trange[1], "%m")),
                 as.integer(format(trange[2], "%Y")), as.integer(format(trange[2], "%m")))
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
  local_stac_api()  # the references are read from the STAC API
  local_mocked_bindings(
    ndc_landuse_raster = function(...) stop("No Land Use raster items matched year 2024.", call. = FALSE),
    ndc_nitrogen_raster = function(...) list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4)),
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
    ndc_landuse_raster = function(...) stop("bad <b>input</b>", call. = FALSE),
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
    ndc_landuse_raster = function(...) stop("No Land Use raster items matched year 2024.", call. = FALSE),
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
                                       "download_summary.csv", "own_polygon.gpkg", "CITATIONS.txt", "parameters"))
    summary <- utils::read.csv(file.path(dir, "download_summary.csv"))
    expect_equal(summary$file_path[summary$date == 2023], "agricultural_fields_geodata_own_polygon_2023.gpkg")
    # the files of the parameters are named after the data files, and say which year each is for
    expect_equal(summary$parameters[summary$date == 2023], "parameters/agricultural_fields_geodata_own_polygon_2023.json")
    expect_setequal(list.files(file.path(dir, "parameters")), c("agricultural_fields_geodata_own_polygon.json",
                                                              "agricultural_fields_geodata_own_polygon_2023.json"))
    years <- vapply(list.files(file.path(dir, "parameters"), full.names = TRUE),
                    function(f) jsonlite::fromJSON(f, simplifyVector = FALSE)$params$year, "")
    expect_setequal(unname(years), c("2023", "2024"))
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
                                       "soil_map_geodata_own_polygon_2.gpkg", "download_summary.csv", "CITATIONS.txt", "parameters"))
    expect_setequal(list.files(file.path(dir, "parameters")), c("soil_map_geodata_own_polygon.json", "soil_map_geodata_own_polygon_2.json"))
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
  local_stac_api()  # the references are read from the STAC API
  rec <- new.env()
  rec$calls <- list()
  local_mocked_bindings(
    retrieve_report_progress = function(detail = NULL, value = NULL) {
      rec$calls[[length(rec$calls) + 1]] <- list(detail = detail, value = value)
    }
  )
  meteo <- sf::st_sf(day = 1, geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326))
  local_mocked_bindings(
    adc_weather_station = function(...) list(closest_id = "260"),
    adc_weather_long_period = function(...) {
      # as the real function does: report before each request (see rNDC::ndc_with_progress())
      for (i in 1:4) getOption("rNDC.progress")(paste0("Downloading chunk ", i, " (", i, "/4)"), i, 4L)
      meteo
    },
    .package = "rNDC"
  )
  rows <- list(overview_row("Land Use", year = 2024L),
               overview_row("Weather", "Statistics", from = date_from <- as.Date("2024-01-01"), to = as.Date("2024-06-30")))
  local_mocked_bindings(ndc_landuse_raster = function(...) list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4)),
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

test_that("the download button only appears once something is in the overview", {
  with_server({
    expect_match(as.character(output$download_ui$html), "Download button will appear here")

    overview(three_rows())
    session$flushReact()
    html <- as.character(output$download_ui$html)
    expect_match(html, "check_and_download", fixed = TRUE)
    expect_match(html, "download_data", fixed = TRUE)
    expect_no_match(html, "return_to_r")  # only offered in interactive sessions
  })
})

test_that("downloading builds the zip once, checks it, and serves it", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("soiltypes")) |>
    webmockr::to_return(body = json_body(geojson_features(1:3, "soiltype")), headers = json_header)

  with_server({
    overview(overview_row("Soil map"))
    expect_null(prepared_zip())

    session$setInputs(check_and_download = 1)
    zip <- prepared_zip()
    expect_true(file.exists(zip))
    expect_setequal(utils::unzip(zip, list = TRUE)$Name,
                    c("soil_map_geodata_own_polygon.gpkg", "own_polygon.gpkg", "download_summary.csv", "CITATIONS.txt",
                      "parameters/soil_map_geodata_own_polygon.json"))

    served <- output$download_data  # the path of the file that would be sent
    expect_true(file.exists(served))
    expect_setequal(utils::unzip(served, list = TRUE)$Name, utils::unzip(zip, list = TRUE)$Name)

    # a second download replaces the first zip
    session$setInputs(check_and_download = 2)
    expect_false(file.exists(zip))
    expect_true(file.exists(prepared_zip()))

    # the zip is removed when the session ends
    last <- prepared_zip()
    session$close()
    expect_false(file.exists(last))
  })
})

test_that("nothing is offered for download when no dataset could be retrieved", {
  local_mocked_bindings(ndc_landuse_raster = function(...) stop("nothing here", call. = FALSE), .package = "rNDC")

  with_server({
    overview(overview_row("Land Use", year = 2024L))
    session$setInputs(check_and_download = 1)
    expect_null(prepared_zip())
  })
})

land_use_overview <- function() overview_row("Land Use", year = 2024L)

# a job manager that runs each job the moment it starts (in this process, so that the mocks of the test apply)
inline_jobs <- function(...) fake_manager(run = TRUE, ...)

test_that("the text for a waiting user gives the position", {
  expect_match(download_queue_text(list(position = 2, queued = 3)), "Position 2 of 3 in the queue")
})

test_that("a download is built in the background", {
  local_stac_api()  # the references are read from the STAC API
  fake <- inline_jobs()
  local_jobs(fake$manager)
  rec <- new.env()
  rec$calls <- 0
  local_mocked_bindings(
    ndc_landuse_raster = function(...) {
      rec$calls <- rec$calls + 1
      list(stack = raster())
    },
    .package = "rNDC"
  )

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    settle(session, function() !retrieving())
    expect_null(job_id())
    expect_length(fake$rec$started, 1)
    expect_true(file.exists(prepared_zip()))
    expect_setequal(utils::unzip(prepared_zip(), list = TRUE)$Name,
                    c("land_use_geodata_own_polygon.tif", "own_polygon.gpkg", "download_summary.csv", "CITATIONS.txt"))
    expect_equal(rec$calls, 1)
    # the tokens reach the process through its environment, not in the arguments
    job <- fake$rec$jobs[[1]]
    expect_equal(job$env, c(NDC_TOKEN = "t", ADC_TOKEN = "t"))
    expect_false(any(c("ndc_token", "adc_token") %in% names(job$args)))
    expect_equal(download_msgs(), "Retrieved: Land Use raster for year 2024")
    expect_length(fake$manager$jobs, 0)  # the job is forgotten
  })
})

test_that("while a retrieval runs the page offers to cancel it, and a second click starts nothing", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))

  with_server({
    overview(land_use_overview())
    expect_false(grepl("Cancel retrieval", as.character(output$download_ui$html)))
    session$setInputs(check_and_download = 1)
    expect_true(retrieving())
    expect_false(is.null(job_id()))
    expect_null(prepared_zip())
    expect_match(as.character(output$download_ui$html), "Cancel retrieval")

    session$setInputs(check_and_download = 2)
    expect_length(fake$rec$started, 1)
    expect_true(any(grepl("already running", rec$notes)))

    # it ends: the progress follows the process and the result is taken
    job_progress_writer(fake$manager$jobs[[job_id()]]$progress_file)(detail = "Land Use", value = 0.4)
    session$elapse(500)
    expect_true(retrieving())
    finish_fake_job(fake, job_id(), value = list(datasets = list(), messages = "m", produced_any = FALSE))
    settle(session, function() !retrieving())
    expect_equal(download_msgs(), c("No data is available within your selection. Please try a different area, time period, or dataset.", "m"))
  })
})

test_that("'Return data to R' gets the data from the background, rasters included", {
  local_jobs(inline_jobs()$manager)
  rec <- new.env()
  local_mocked_bindings(ndc_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  local_mocked_bindings(return_data_to_r = function(res) rec$res <- res)

  with_server({
    overview(land_use_overview())
    session$setInputs(return_to_r = 1)
    settle(session, function() !is.null(rec$res))
    expect_s4_class(rec$res$datasets$`Land Use_1`, "SpatRaster")
    expect_equal(terra::values(rec$res$datasets$`Land Use_1`)[, 1], 1:4)
    expect_true(rec$res$produced_any)
    expect_false(retrieving())
  })
})

test_that("a retrieval that fails in the background is reported and the session can retrieve again", {
  local_jobs(inline_jobs()$manager)
  local_mocked_bindings(retrieve_row = function(...) stop("worker boom", call. = FALSE))
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    settle(session, function() !retrieving())
    expect_null(prepared_zip())
    expect_null(job_id())
  })
  expect_true(any(grepl("The retrieval failed: worker boom", rec$notes)))
})

test_that("the user can cancel a retrieval that runs", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))
  zips <- function() list.files(tempdir(), "\\.zip$")
  before <- zips()

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    id <- job_id()
    expect_equal(job_status(fake$manager, id)$state, "running")
    expect_match(as.character(output$download_ui$html), "Cancel retrieval")

    session$setInputs(cancel_retrieval = 1)
    settle(session, function() !retrieving())
    expect_equal(fake$rec$killed, id)
    expect_null(job_id())
    expect_null(prepared_zip())
    expect_match(as.character(output$download_ui$html), "Download dataset")
    expect_false(grepl("Cancel retrieval", as.character(output$download_ui$html)))

    # and a new retrieval can be started
    session$setInputs(check_and_download = 2)
    expect_false(is.null(job_id()))
    expect_equal(length(fake$rec$started), 2)
  })
  expect_true(any(grepl("cancelled", rec$notes)))
  expect_identical(zips(), before)
})

test_that("a retrieval waits in the queue while the workers are busy, and cancelling it needs no kill", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)
  busy <- job_submit(fake$manager, "f", list())  # someone else's job occupies the only worker

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    id <- job_id()
    st <- job_status(fake$manager, id)
    expect_equal(st$state, "queued")
    expect_equal(c(st$position, st$queued), c(1, 1))
    expect_true(retrieving())

    # the other job ends: this one starts
    finish_fake_job(fake, busy, value = NULL)
    settle(session, function() identical(job_status(fake$manager, id)$state, "running"))
    expect_equal(fake$rec$started, c(busy, id))

    # one more user queues, the first one gives up before its turn
    session$setInputs(cancel_retrieval = 1)
    settle(session, function() !retrieving())
    expect_equal(fake$rec$killed, id)
  })
})

test_that("a waiting retrieval is cancelled without a process to kill", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)
  busy <- job_submit(fake$manager, "f", list())

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    expect_equal(job_status(fake$manager, job_id())$state, "queued")
    session$setInputs(cancel_retrieval = 1)
    settle(session, function() !retrieving())
    expect_length(fake$rec$killed, 0)
    expect_equal(fake$rec$started, busy)
  })
})

test_that("a full queue refuses the retrieval with a message", {
  fake <- fake_manager(workers = 1, max_queue = 0)
  local_jobs(fake$manager)
  job_submit(fake$manager, "f", list())
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    expect_false(retrieving())
    expect_null(job_id())
  })
  expect_true(any(grepl("server is busy", rec$notes)))
})

test_that("a retrieval that runs when the user leaves is cancelled, and nothing is left behind", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)
  zips <- function() list.files(tempdir(), "\\.zip$")
  before <- zips()

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    id <- job_id()
    session$close()
    expect_equal(fake$rec$killed, id)
  })
  expect_length(fake$manager$jobs, 0)
  expect_identical(zips(), before)
})

test_that("a download that finished when the user leaves does not leave a zip behind", {
  fake <- fake_manager(workers = 1)
  local_jobs(fake$manager)

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    id <- job_id()
    zip <- fake$manager$jobs[[id]]$args$zipfile
    writeLines("zip", zip)  # what the job made
    finish_fake_job(fake, id, value = list(datasets = list(), messages = character(0), produced_any = TRUE))
    session$close()  # before the page has looked at the job
    expect_false(file.exists(zip))
  })
  expect_length(fake$manager$jobs, 0)
  expect_length(fake$rec$killed, 0)  # nothing to stop
})

test_that("without the background mode a download is built at once, as before", {
  local_mocked_bindings(ndc_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  expect_false(job_async_enabled())

  with_server({
    overview(land_use_overview())
    session$setInputs(check_and_download = 1)
    expect_false(retrieving())
    expect_null(job_id())
    expect_true(file.exists(prepared_zip()))  # no waiting
  })
})
