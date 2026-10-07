# Estimating how long the overview takes, warning or refusing, and the progress while it is retrieved.

d <- as.Date

test_that("the estimate counts the requests of Weather and of the NDVI rasters, and nothing else", {
  est <- function(...) estimate_row_seconds(...)
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
  expect_equal(est(c("AHN", "Soil map", "Land Use", "Nitrogen", "Agricultural fields"), "Geodata", d(NA), d(NA)),
               rep(0, 5))

  # vectorised over the rows of an overview, also an empty one
  expect_equal(est(c("Weather", "AHN", "NDVI"), c("Statistics", "Statistics", "Geodata"),
                   c(d("2024-05-01"), d(NA), d("2025-06-01")), c(d("2024-05-01"), d(NA), d("2025-06-30"))),
               c(w, 0, 30 * n))
  expect_equal(est(character(), character(), d(character()), d(character())), numeric())
})

test_that("the limits fit the measured request times", {
  # the longest Weather period (since 1970) is allowed, a few years of NDVI rasters is warned about,
  # and the whole NDVI record is refused
  weather <- estimate_row_seconds("Weather", "Statistics", weather_min_date, Sys.Date())
  expect_lt(weather, max_request_seconds())
  expect_gt(weather, warn_request_seconds)
  expect_lt(estimate_row_seconds("NDVI", "Geodata", d("2025-06-01"), d("2025-06-30")), warn_request_seconds)
  expect_gt(estimate_row_seconds("NDVI", "Geodata", d("2018-01-01"), d("2019-12-31")), warn_request_seconds)
  expect_gt(estimate_row_seconds("NDVI", "Geodata", ndvi_min_month, Sys.Date()), max_request_seconds())
})

test_that("the limit can be set with an option", {
  expect_equal(max_request_seconds(), 300)
  withr::local_options(rNDC.Shiny.max_request_seconds = 1000)
  expect_equal(max_request_seconds(), 1000)
})

test_that("durations are described in minutes", {
  expect_equal(format_duration(5), "less than a minute")
  expect_equal(format_duration(59.9), "less than a minute")
  expect_equal(format_duration(60), "about 1 minute")
  expect_equal(format_duration(95), "about 2 minutes")
  expect_equal(format_duration(300), "about 5 minutes")
  expect_match(long_retrieval_warning(182), "about 3 minutes")
  expect_match(too_long_retrieval_message(700), "about 12 minutes.*limit of about 5 minutes")
})

test_that("a report with a count moves the bar; any report becomes the detail", {
  p <- progress_from_report("Downloading 2024-01-01 -> 2024-07-18 (1/4)", 1L, 4L, "Weather (A)", index = 1, n = 1)
  expect_equal(p$value, 0)  # the first of four requests is starting
  expect_equal(p$detail, "Weather (A): Downloading 2024-01-01 -> 2024-07-18 (1/4)")

  expect_equal(progress_from_report("x", 3L, 4L, "W", 1, 1)$value, 0.5)
  # the bar covers all the rows: the 3rd of 4 requests of the 2nd of 5 rows
  expect_equal(progress_from_report("x", 3L, 4L, "W", 2, 5)$value, (1 + 2 / 4) / 5)
  expect_equal(progress_from_report("x\n", 4L, 4L, "W", 1, 1)$value, 0.75)  # trailing newline

  none <- progress_from_report("Skipping 20240520", NA_integer_, NA_integer_, "NDVI (A)", 1, 2)
  expect_null(none$value)
  expect_equal(none$detail, "NDVI (A): Skipping 20240520")

  long <- progress_from_report(strrep("x", 200), 1L, 2L, "L", 1, 1)
  expect_lte(nchar(long$detail), 75)
  expect_match(long$detail, "\\.\\.\\.$")

  expect_null(progress_from_report("  \n", 1L, 2L, "L", 1, 1)$detail)
})

test_that("what the rNDC functions report is shown as progress", {
  rec <- new.env()
  rec$calls <- list()
  local_mocked_bindings(report_progress = function(detail = NULL, value = NULL) {
    rec$calls[[length(rec$calls) + 1]] <- list(detail = detail, value = value)
  })

  # the contract with rNDC: a function reports to the option rNDC.progress (as rNDC::get_meteo_for_long_period does)
  result <- with_request_progress({
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
  local_mocked_bindings(report_progress = function(detail = NULL, value = NULL) rec$values <- c(rec$values, value))
  local_mocked_bindings(
    get_meteo_for_period = function(...) sf::st_sf(a = 1, geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)),
    .package = "rNDC"
  )
  suppressMessages(with_request_progress(
    rNDC::get_meteo_for_long_period(310, "2024-01-01", "2024-01-20", token = "t", by_days = 7), "Weather (A)", 1, 1
  ))
  expect_equal(rec$values, c(0, 1 / 3, 2 / 3))
})

test_that("a flood of reports without a count updates the page only now and then", {
  rec <- new.env()
  rec$n <- 0
  local_mocked_bindings(report_progress = function(detail = NULL, value = NULL) rec$n <- rec$n + 1)

  with_request_progress({
    report <- getOption("rNDC.progress")
    for (i in 1:500) report(paste("Skipping day", i), NA_integer_, NA_integer_)
  }, "NDVI (A)", 1, 1)
  expect_lt(rec$n, 10)
  expect_gte(rec$n, 1)
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
