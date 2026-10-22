# Save parameters (JSON): the files of rNDC::ndc_repeat() for the datasets of the overview, written without retrieving them.
# (R/export_parameters.R, and the button of R/server_download.R)

date <- as.Date

# The rows of an overview with one of each dataset and view (the NDVI statistics for a project area)
all_rows <- function() {
  project <- overview_row("NDVI", "Statistics", from = date("2024-01-01"), to = date("2024-12-31"), name = "Light on Nature_3")
  project$polygon_sf[[1]]$ndc_id <- "1264502"
  dplyr::bind_rows(
    overview_row("Land Use", year = 2024L),
    overview_row("Nitrogen", year = 2025L),
    overview_row("AHN", "Geodata"),
    overview_row("AHN", "Statistics"),
    overview_row("Soil map"),
    overview_row("Agricultural fields", year = 2023L),
    overview_row("NDVI", "Geodata", from = date("2024-06-01"), to = date("2024-06-30")),
    overview_row("NDVI", "Geodata", from = date("2024-05-01"), to = date("2024-07-31")),
    project,
    overview_row("Weather", "Statistics", from = date("2024-06-01"), to = date("2024-06-07")),
    overview_row("Weather", "Statistics", from = date("2024-06-01"), to = date("2024-06-01"), name = "Other polygon")
  )
}

test_that("a row stands for the call of its dataset: the function and the arguments that its file records", {
  rows <- all_rows()
  fun_of <- function(i) vapply(row_calls(rows[i, ], "275"), function(cl) cl$fun, "")
  expect_equal(fun_of(1), "ndc_landuse_raster")
  expect_equal(fun_of(2), "ndc_nitrogen_raster")
  expect_equal(fun_of(3), "ndc_ahn_raster")
  expect_equal(fun_of(4), "ndc_ahn_stats")
  expect_equal(fun_of(5), "ndc_soil_raster")
  expect_equal(fun_of(6), "adc_get_all")
  expect_equal(fun_of(7), "ndc_ndvi_month")                                 # one month ...
  expect_equal(fun_of(8), "ndc_ndvi_stack")                                 # ... or several
  expect_equal(fun_of(9), "ndc_ndvi_stats")
  expect_equal(fun_of(10), c("adc_weather_station", "adc_weather_long_period"))
  expect_equal(fun_of(11), c("adc_weather_station", "adc_weather_date"))    # a single day

  nitrogen <- row_calls(rows[2, ])[[1]]$args
  expect_equal(nitrogen$year, "2025")
  expect_equal(nitrogen$layers, c("ntot", "nox", "nh3"))
  expect_equal(row_calls(rows[7, ])[[1]]$args[c("year", "month")], list(year = 2024L, month = 6L))
  stats <- row_calls(rows[9, ])[[1]]$args
  expect_equal(stats$collection, "ndvi-lter")
  expect_equal(stats$ndc_id, "1264502")
  expect_equal(stats$trange, date(c("2024-01-01", "2024-12-31")))
  fields <- row_calls(rows[6, ])[[1]]$args
  expect_equal(fields$option, "Fields")
  expect_match(fields$params[["geometry"]], "^POLYGON")
  expect_equal(fields$params[["year"]], "2023")

  # Weather has the call for its data only once the station is known; others have nothing to say
  expect_equal(vapply(row_calls(rows[10, ]), function(cl) cl$slot, ""), "station")
  expect_equal(vapply(row_calls(rows[10, ], "275"), function(cl) cl$slot, ""), c("station", "data"))
  expect_length(row_calls(overview_row("NDVI", "Statistics", from = date("2024-01-01"), to = date("2024-12-31"))), 0)   # no project
  expect_length(row_calls(overview_row("Vegetation structure")), 0)
})

# --- the calls are those of the retrievals ---

# Run the retrieval of `row` with the rNDC functions replaced by recorders; the arguments of each call that its file records
recorded_calls <- function(row) {
  rec <- new.env()
  rec$calls <- list()
  specs <- getFromNamespace("params_specs", "rNDC")
  record <- function(fun) {
    force(fun)
    function(...) {
      args <- list(...)
      if (!is.null(names(args)) || length(args) > 0) {
        # positional arguments take the names of the function (as when it is called)
        f <- getFromNamespace(fun, "rNDC")
        args <- as.list(match.call(f, as.call(c(as.name(fun), args)))[-1])
      }
      rec$calls[[length(rec$calls) + 1]] <- list(fun = fun, args = args[intersect(names(args), names(specs[[fun]]$args))])
      if (fun == "adc_weather_station") return(list(closest_id = "275", distances = 1))
      stop("recorded")
    }
  }
  funs <- c("adc_get_all", "ndc_landuse_raster", "ndc_nitrogen_raster", "ndc_ahn_raster", "ndc_ahn_stats", "ndc_soil_raster",
            "ndc_ndvi_month", "ndc_ndvi_stack", "ndc_ndvi_stats", "adc_weather_station", "adc_weather_date", "adc_weather_long_period")
  mocks <- stats::setNames(lapply(funs, record), funs)
  do.call(testthat::local_mocked_bindings, c(mocks, list(.package = "rNDC", .env = environment())))
  try(retrieve_dataset(row, 1, "ndc-token", "adc-token", stem = NULL), silent = TRUE)
  rec$calls
}

test_that("the arguments of the files are those that the retrievals use: the two cannot drift apart", {
  rows <- all_rows()
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    label <- paste(row$dataset, row$view, row$polygon)
    retrieved <- recorded_calls(row)
    expected <- row_calls(row, "275")
    expect_equal(vapply(retrieved, function(cl) cl$fun, ""), vapply(expected, function(cl) cl$fun, ""), info = label)
    for (k in seq_along(expected)) {
      for (arg in names(retrieved[[k]]$args)) {
        # what the retrieval gives the function (and its file records) is in the file too
        value <- retrieved[[k]]$args[[arg]]
        wanted <- expected[[k]]$args[[arg]]
        if (is.null(value)) next
        if (arg == "year") { wanted <- as.character(wanted); value <- as.character(value) }   # (the function writes the year as text)
        expect_equal(wanted, value, info = paste(label, arg))
      }
    }
  }
})

# --- the files ---

test_that("the files of an overview are written with the names of the download, one per call, and run with ndc_repeat()", {
  rows <- all_rows()
  local_mocked_bindings(adc_weather_station = function(roi, ...) list(closest_id = "275", distances = 1), .package = "rNDC")
  dir <- withr::local_tempdir()
  export <- parameters_export(rows, dir, "adc-token")

  expect_equal(nrow(export$files), 13)                                 # 11 rows, and Weather has two files
  expect_equal(sort(list.files(dir)), sort(export$files$file))
  expect_equal(export$files$file[1:3], c("land_use_geodata_own_polygon.json", "nitrogen_geodata_own_polygon.json",
                                         "ahn_geodata_own_polygon.json"))
  expect_true(all(c("weather_statistics_own_polygon_station.json", "weather_statistics_own_polygon.json",
                    "weather_statistics_other_polygon_station.json") %in% export$files$file))
  expect_equal(anyDuplicated(export$files$file), 0)
  expect_length(export$notes, 0)

  # every file is that of its function, and has no token
  for (i in seq_len(nrow(export$files))) {
    text <- paste(readLines(file.path(dir, export$files$file[i])), collapse = "\n")
    recorded <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    expect_equal(recorded$schema, paste0("rNDC-", export$files$fun[i], "/1"), info = export$files$file[i])
    expect_false(grepl("token|adc-token|ndc-token", text, ignore.case = TRUE), info = export$files$file[i])
  }
  weather <- jsonlite::fromJSON(file.path(dir, "weather_statistics_own_polygon.json"), simplifyVector = FALSE)
  expect_equal(c(weather$station, weather$trange), c("275", "2024-06-01T00:00:00Z/2024-06-07T00:00:00Z"))
  expect_equal(jsonlite::fromJSON(file.path(dir, "weather_statistics_other_polygon.json"))$date, "2024-06-01")

  # ndc_repeat() runs a file: the function of the file is called with it
  local_mocked_bindings(ndc_ahn_stats = function(params_file = NULL, token, ...) list(params_file = params_file, token = token),
                        .package = "rNDC")
  ran <- rNDC::ndc_repeat(file.path(dir, "ahn_statistics_own_polygon.json"), token = "t")
  expect_equal(basename(ran$params_file), "ahn_statistics_own_polygon.json")
})

test_that("a file from the overview is run by the function that it names, with the area and the year of the row", {
  local_stac_api(pages = list(list(raster_item("a", "2024-01-01"))))
  local_mocked_bindings(stac_download_one = function(href, outfile, ...) {
    terra::writeRaster(terra::rast(nrows = 2, ncols = 2, xmin = 5.7, xmax = 5.8, ymin = 52, ymax = 52.1, crs = "EPSG:4326", vals = 1:4),
                       outfile, overwrite = TRUE)
    outfile
  }, .package = "rNDC")
  dir <- withr::local_tempdir()
  parameters_export(overview_row("Land Use", year = 2024L), dir)
  res <- rNDC::ndc_repeat(file.path(dir, "land_use_geodata_own_polygon.json"), token = "t", output_dir = withr::local_tempdir(),
                         min_file_size = 0)       # (what is given overrides the file: the fake raster is small)
  expect_equal(names(res$stack), "LGN_2024")
})

test_that("rows with the same name get the period, or a number; and the rows without a file say why", {
  one <- overview_row("Land Use", year = 2024L)
  rows <- dplyr::bind_rows(one, transform(one, year = 2020L), transform(one, year = 2020L), transform(one, year = NA_integer_))
  expect_equal(parameter_base_names(rows), c("land_use_geodata_own_polygon", "land_use_geodata_own_polygon_2020",
                                             "land_use_geodata_own_polygon_2", "land_use_geodata_own_polygon_3"))

  rows <- dplyr::bind_rows(overview_row("NDVI", "Statistics", from = date("2024-01-01"), to = date("2024-12-31")),
                           overview_row("Weather", "Statistics", from = date("2024-06-01"), to = date("2024-06-07")))
  local_mocked_bindings(adc_weather_station = function(roi, ...) list(closest_id = character(0), distances = 1), .package = "rNDC")
  export <- parameters_export(rows, withr::local_tempdir(), "adc-token")
  expect_equal(export$files$file, "weather_statistics_own_polygon_station.json")           # only the station
  expect_length(export$notes, 2)
  expect_match(export$notes[1], "NDVI, Statistics, Own polygon: no file (NDVI statistics exist for the LTER and SNL project areas only).", fixed = TRUE)
  expect_match(export$notes[2], "the weather station could not be found", fixed = TRUE)
  text <- parameters_readme(export)
  expect_true(any(grepl("ndc_repeat(\"weather_statistics_own_polygon_station.json\"", text, fixed = TRUE)))
  expect_true(any(grepl("Not written:", text, fixed = TRUE)))
})

test_that("the file count is the number of files, without asking the services", {
  rows <- all_rows()
  expect_equal(parameter_file_count(rows), 13)
  expect_equal(parameter_file_count(rows[1, ]), 1)
  expect_equal(parameter_file_count(rows[10, ]), 2)
})

# --- the button ---

test_that("the button is next to Download dataset(s), and gives one JSON file for one dataset, otherwise a zip", {
  local_clean_cache()
  with_server({
    expect_no_match(as.character(output$download_ui$html), "download_parameters")        # nothing in the overview
    overview(overview_row("Land Use", year = 2024L))
    session$flushReact()
    html <- as.character(output$download_ui$html)
    expect_match(html, "Save parameters (JSON)", fixed = TRUE)
    expect_lt(regexpr('id="check_and_download"', html, fixed = TRUE), regexpr('id="download_parameters"', html, fixed = TRUE))

    single <- output$download_parameters
    expect_equal(basename(single) |> tools::file_ext(), "json")
    expect_equal(jsonlite::fromJSON(single)$schema, "rNDC-ndc_landuse_raster/1")
    expect_equal(jsonlite::fromJSON(single)$year, 2024)

    overview(three_rows())
    session$flushReact()
    zip_file <- output$download_parameters
    expect_equal(tools::file_ext(zip_file), "zip")
    listing <- utils::unzip(zip_file, list = TRUE)$Name
    expect_setequal(listing, c("ahn_statistics_own_polygon.json", "soil_map_geodata_own_polygon.json",
                               "land_use_geodata_other_polygon.json", "README.txt"))
    dir <- withr::local_tempdir()
    utils::unzip(zip_file, exdir = dir)
    expect_match(paste(readLines(file.path(dir, "README.txt")), collapse = "\n"), "ndc_repeat(", fixed = TRUE)
    expect_equal(nrow(overview()), 3)                                                    # (nothing was retrieved or changed)
  })
})

test_that("Weather in the overview asks for the station, and a zip has both of its files", {
  local_clean_cache()
  asked <- new.env()
  asked$roi <- character()
  local_mocked_bindings(adc_weather_station = function(roi, token, ...) {
    asked$roi <- c(asked$roi, roi)
    list(closest_id = "275", distances = 1)
  }, .package = "rNDC")
  with_server({
    overview(overview_row("Weather", "Statistics", from = date("2024-06-01"), to = date("2024-06-07")))
    session$flushReact()
    zip_file <- output$download_parameters
    expect_equal(tools::file_ext(zip_file), "zip")
    expect_setequal(utils::unzip(zip_file, list = TRUE)$Name,
                    c("weather_statistics_own_polygon_station.json", "weather_statistics_own_polygon.json", "README.txt"))
  })
  expect_length(asked$roi, 1)                                                            # one request for the station
  expect_match(asked$roi, "^POLYGON")
})
