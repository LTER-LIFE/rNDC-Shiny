# Estimating how long the overview takes, warning or refusing, and the progress shown while it is retrieved. (R/retrieve_cost.R)

d <- as.Date

test_that("the estimate counts the requests of Weather and of the NDVI rasters, and nothing else", {
  est <- function(...) retrieve_estimate_seconds(...)
  w <- request_seconds[["weather_chunk"]]
  n <- request_seconds[["ndvi_day"]]

  # Weather: one request per chunk of days, at least one
  expect_equal(est("Weather", "Statistics", d("2024-05-01"), d("2024-05-01")), w)
  expect_equal(est("Weather", "Statistics", d("2024-01-01"), d("2024-07-18")), w)        # 200 days
  expect_equal(est("Weather", "Statistics", d("2024-01-01"), d("2024-07-19")), 2 * w)    # 201 days
  expect_equal(est("Weather", "Statistics", d("2020-01-01"), d("2024-12-31")), 10 * w)   # 1827 days
  expect_equal(est("Weather", "Statistics", d(NA), d(NA)), w)

  # NDVI rasters: one download per day; the statistics come from one STAC request
  expect_equal(est("NDVI", "Geodata", d("2025-06-01"), d("2025-06-30")), 30 * n)
  expect_equal(est("NDVI", "Geodata", d("2025-06-01"), d("2025-08-31")), 92 * n)
  expect_equal(est("NDVI", "Statistics", d("2017-05-01"), d("2025-12-31")), 0)
  expect_equal(est("NDVI", "Geodata", d(NA), d(NA)), 0)

  # everything else is quick
  expect_equal(est(c("Land Use", "Nitrogen", "Agricultural fields"), "Geodata", d(NA), d(NA)), rep(0, 3))

  # vectorised over the rows of an overview, also an empty one
  expect_equal(est(c("Weather", "Land Use", "NDVI"), c("Statistics", "Statistics", "Geodata"),
                   c(d("2024-05-01"), d(NA), d("2025-06-01")), c(d("2024-05-01"), d(NA), d("2025-06-30"))),
               c(w, 0, 30 * n))
  expect_equal(est(character(), character(), d(character()), d(character())), numeric())
})

test_that("the limits fit the measured request times", {
  # the longest Weather period (since 1970) is allowed, a few years of NDVI rasters is warned about,
  # and the whole NDVI record is refused
  weather <- retrieve_estimate_seconds("Weather", "Statistics", weather_min_date, Sys.Date())
  expect_lt(weather, max_request_seconds())
  expect_gt(weather, warn_request_seconds)
  expect_lt(retrieve_estimate_seconds("NDVI", "Geodata", d("2025-06-01"), d("2025-06-30")), warn_request_seconds)
  expect_gt(retrieve_estimate_seconds("NDVI", "Geodata", d("2018-01-01"), d("2019-12-31")), warn_request_seconds)
  expect_gt(retrieve_estimate_seconds("NDVI", "Geodata", ndvi_min_month, Sys.Date()), max_request_seconds())
})

test_that("the limit can be set with an option", {
  expect_equal(max_request_seconds(), 300)
  withr::local_options(rNDC.Shiny.max_request_seconds = 1000)
  expect_equal(max_request_seconds(), 1000)
})

test_that("durations are described in minutes", {
  expect_equal(retrieve_format_duration(5), "less than a minute")
  expect_equal(retrieve_format_duration(59.9), "less than a minute")
  expect_equal(retrieve_format_duration(60), "about 1 minute")
  expect_equal(retrieve_format_duration(95), "about 2 minutes")
  expect_equal(retrieve_format_duration(300), "about 5 minutes")
  expect_match(retrieve_long_warning(182), "about 3 minutes")
  expect_match(retrieve_too_long_message(700), "about 12 minutes.*limit of about 5 minutes")
})

test_that("NDC_MAX_REQUEST_SECONDS sets the limit; nonsense is ignored", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t", SHINY_APP_BASE_URL = NA)
  withr::local_options(rNDC.Shiny.max_request_seconds = NULL)

  withr::local_envvar(NDC_MAX_REQUEST_SECONDS = "120")
  ndc_setup()
  expect_equal(max_request_seconds(), 120)

  options(rNDC.Shiny.max_request_seconds = NULL)
  for (bad in c("abc", "-5", "0", "")) {
    withr::local_envvar(NDC_MAX_REQUEST_SECONDS = bad)
    ndc_setup()
    expect_equal(max_request_seconds(), 300, info = bad)
  }
})

test_that("the longer limit and the wording follow the mode", {
  expect_equal(max_request_seconds(), 300)
  expect_match(retrieve_long_warning(120), "busy until it is done")

  withr::local_options(rNDC.Shiny.async = TRUE)
  expect_equal(max_request_seconds(), 900)
  expect_match(retrieve_long_warning(120), "runs in the background")
  expect_match(retrieve_long_warning(120), "cancel")

  withr::local_options(rNDC.Shiny.max_request_seconds = 100)  # an explicit limit always wins
  expect_equal(max_request_seconds(), 100)
})

# ---- progress ----


test_that("a report with a count moves the bar; any report becomes the detail", {
  p <- retrieve_progress_from_report("Downloading 2024-01-01 -> 2024-07-18 (1/4)", 1L, 4L, "Weather (A)", index = 1, n = 1)
  expect_equal(p$value, 0)  # the first of four requests is starting
  expect_equal(p$detail, "Weather (A): Downloading 2024-01-01 -> 2024-07-18 (1/4)")

  expect_equal(retrieve_progress_from_report("x", 3L, 4L, "W", 1, 1)$value, 0.5)
  # the bar covers all the rows: the 3rd of 4 requests of the 2nd of 5 rows
  expect_equal(retrieve_progress_from_report("x", 3L, 4L, "W", 2, 5)$value, (1 + 2 / 4) / 5)
  expect_equal(retrieve_progress_from_report("x\n", 4L, 4L, "W", 1, 1)$value, 0.75)  # trailing newline

  none <- retrieve_progress_from_report("Skipping 20240520", NA_integer_, NA_integer_, "NDVI (A)", 1, 2)
  expect_null(none$value)
  expect_equal(none$detail, "NDVI (A): Skipping 20240520")

  long <- retrieve_progress_from_report(strrep("x", 200), 1L, 2L, "L", 1, 1)
  expect_lte(nchar(long$detail), 75)
  expect_match(long$detail, "\\.\\.\\.$")

  expect_null(retrieve_progress_from_report("  \n", 1L, 2L, "L", 1, 1)$detail)
})

test_that("what the rNDC functions report is shown as progress", {
  rec <- new.env()
  rec$calls <- list()
  local_mocked_bindings(retrieve_report_progress = function(detail = NULL, value = NULL) {
    rec$calls[[length(rec$calls) + 1]] <- list(detail = detail, value = value)
  })

  # the contract with rNDC: a function reports to the option rNDC.progress (as rNDC::adc_weather_long_period does)
  result <- retrieve_with_progress({
    report <- getOption("rNDC.progress")
    report("Downloading a (1/2)", 1L, 2L)
    report("Downloading b (2/2)", 2L, 2L)
    "done"
  }, "Weather (A)", 1, 1)
  expect_equal(result, "done")
  expect_equal(vapply(rec$calls, function(x) x$value, numeric(1)), c(0, 0.5))
  expect_equal(rec$calls[[2]]$detail, "Weather (A): Downloading b (2/2)")
  expect_null(getOption("rNDC.progress"))  # only while the code runs
})

test_that("a weather request of rNDC moves the bar", {
  rec <- new.env()
  rec$values <- numeric(0)
  local_mocked_bindings(retrieve_report_progress = function(detail = NULL, value = NULL) rec$values <- c(rec$values, value))
  local_mocked_bindings(
    adc_weather_period = function(...) sf::st_sf(a = 1, geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)),
    .package = "rNDC"
  )
  suppressMessages(retrieve_with_progress(
    rNDC::adc_weather_long_period(310, c("2024-01-01", "2024-01-20"), token = "t", by_days = 7), "Weather (A)", 1, 1
  ))
  expect_equal(rec$values, c(0, 1 / 3, 2 / 3))
})

test_that("a flood of reports without a count updates the page only now and then", {
  rec <- new.env()
  rec$n <- 0
  local_mocked_bindings(retrieve_report_progress = function(detail = NULL, value = NULL) rec$n <- rec$n + 1)

  retrieve_with_progress({
    report <- getOption("rNDC.progress")
    for (i in 1:500) report(paste("Skipping day", i), NA_integer_, NA_integer_)
  }, "NDVI (A)", 1, 1)
  expect_lt(rec$n, 10)
  expect_gte(rec$n, 1)
})

# a polygon (EPSG:4326) of about `km` x `km` kilometres near Utrecht
square_km <- function(km) {
  x <- 5.2; y <- 52.0
  sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = x, ymin = y, xmax = x + km / 68.5, ymax = y + km / 111.2), crs = sf::st_crs(4326))))
}

test_that("the AHN and the soil map take longer for a larger area, up to the cells that rNDC requests", {
  r <- request_seconds[["raster_request"]]
  m <- request_seconds[["raster_million_cells"]]
  est <- function(dataset, km, view = "Geodata") retrieve_estimate_seconds(dataset, view, d(NA), d(NA), list(square_km(km)))

  # a small area: a request for each raster (two for the AHN)
  expect_equal(est("Soil map", 1), r + m * (1e6 / 25) / 1e6, tolerance = 0.01)
  expect_equal(est("AHN", 1), 2 * r + m * (1e6 / 25 + 1e6 / 6.25) / 1e6, tolerance = 0.01)
  expect_equal(est("AHN", 1, "Statistics"), est("AHN", 1))
  # the measured times of an area of 2100 km2: 16 s for each AHN raster and 13 s for the soil map
  expect_equal(est("AHN", 46), 32, tolerance = 0.2)
  expect_equal(est("Soil map", 46), 13, tolerance = 0.2)
  # more area, more time, but not beyond the cells that are requested
  expect_gt(est("AHN", 10), est("AHN", 1))
  expect_equal(est("AHN", 200), 2 * (r + m * raster_max_cells / 1e6))
  # without the polygons a raster counts as a request
  expect_equal(retrieve_estimate_seconds("Soil map", "Geodata", d(NA), d(NA)), r)
  expect_equal(retrieve_raster_seconds(5, NULL), r)
  # vectorised, with the polygons of each row
  expect_equal(retrieve_estimate_seconds(c("AHN", "Land Use", "Soil map"), "Geodata", d(rep(NA, 3)), d(rep(NA, 3)),
                                         list(square_km(1), square_km(1), square_km(1))),
               c(est("AHN", 1), 0, est("Soil map", 1)))
})

test_that("a retrieval of the largest area allowed is warned about, but not refused", {
  seconds <- sum(retrieve_estimate_seconds(c("AHN", "AHN", "Soil map"), c("Statistics", "Geodata", "Geodata"),
                                           d(rep(NA, 3)), d(rep(NA, 3)), rep(list(square_km(46)), 3)))
  expect_gt(seconds, warn_request_seconds)
  expect_lt(seconds, max_request_seconds())
})
