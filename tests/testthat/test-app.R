# Smoke tests: the package loads, the app can be created and the server logic starts.
# (More thorough, mocked-HTTP tests follow.)

test_that("ndc_setup() requires NDC_TOKEN and warns about a missing ADC_TOKEN", {
  withr::local_envvar(NDC_TOKEN = NA, ADC_TOKEN = NA, SHINY_APP_BASE_URL = NA)
  expect_error(ndc_setup(), "NDC_TOKEN")

  withr::local_envvar(NDC_TOKEN = "t")
  expect_warning(ndc_setup(), "ADC_TOKEN")

  withr::local_envvar(ADC_TOKEN = "t")
  expect_no_warning(ndc_setup())
})

test_that("ndc_setup() sets the proxy path from SHINY_APP_BASE_URL", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t", SHINY_APP_BASE_URL = "/naturedatacube")
  withr::local_options(shiny.appBaseUrl = NULL)
  ndc_setup()
  expect_identical(getOption("shiny.appBaseUrl"), "/naturedatacube")
})

test_that("ndc_app() returns a Shiny app and the bundled resources exist", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t")
  expect_s3_class(ndc_app(), "shiny.appobj")
  expect_true(file.exists(system.file("app", "www", "LTER-LIFE-logo.png", package = "rNDC.Shiny")))
  expect_true(file.exists(system.file("app", "NatureDataCube_README.txt", package = "rNDC.Shiny")))
})

test_that("the user interface can be built", {
  expect_s3_class(app_ui(), "shiny.tag.list")
})

test_that("datasets that need an ADC token cannot be added without one", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "")
  wkt <- "POLYGON((5.75 52.05,5.76 52.05,5.76 52.06,5.75 52.06,5.75 52.05))"
  sel <- sf::st_sf(wkt = wkt, source_name = "Own polygon", layer_id = 1L,
                   geometry = sf::st_as_sfc(wkt, crs = 4326))

  shiny::testServer(app_server, {
    # Select the dataset first: the first flush runs the initial observers, which reset the selection.
    session$setInputs(selected_dataset = "Weather", weather_mode = "single",
                      weather_date = Sys.Date() - 2)
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    session$flushReact()
    expect_equal(nrow(overview()), 0)

    session$setInputs(selected_dataset = "Land Use")
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 2)
    session$flushReact()
    expect_equal(nrow(overview()), 1)
  })
})

test_that("NDVI months are checked when a dataset is added", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t")
  wkt <- "POLYGON((5.75 52.05,5.76 52.05,5.76 52.06,5.75 52.06,5.75 52.05))"
  sel <- sf::st_sf(wkt = wkt, source_name = "Own polygon", layer_id = 1L,
                   geometry = sf::st_as_sfc(wkt, crs = 4326))

  shiny::testServer(app_server, {
    session$setInputs(selected_dataset = "NDVI", ndvi_mode = "range",
                      ndvi_from_year = 2025, ndvi_from_month = 6,
                      ndvi_to_year = 2025, ndvi_to_month = 3)
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)  # start after end: rejected
    session$flushReact()
    expect_equal(nrow(overview()), 0)

    session$setInputs(ndvi_to_month = 8)
    session$setInputs(add_dataset = 2)
    session$flushReact()
    expect_equal(nrow(overview()), 1)
    expect_equal(overview()$date_from, as.Date("2025-06-01"))
    expect_equal(overview()$date_to, as.Date("2025-08-31"))
  })
})

test_that("NDVI: a start in the future is rejected and an end in the future is clipped", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t")
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  sel <- selected_polygon()

  shiny::testServer(app_server, {
    session$setInputs(selected_dataset = "NDVI", ndvi_mode = "range",
                      ndvi_from_year = this_year + 1, ndvi_from_month = 1,
                      ndvi_to_year = this_year + 1, ndvi_to_month = 12)
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    session$flushReact()
    expect_equal(nrow(overview()), 0)

    session$setInputs(ndvi_from_year = this_year, ndvi_from_month = 1,
                      ndvi_to_year = this_year + 1, ndvi_to_month = 12)
    session$setInputs(add_dataset = 2)
    session$flushReact()
    expect_equal(overview()$date_from, as.Date(paste0(this_year, "-01-01")))
    expect_equal(overview()$date_to, lubridate::ceiling_date(Sys.Date(), "month") - 1)
  })
})

test_that("invalid NDVI months and empty inputs are rejected", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t")
  sel <- selected_polygon()

  shiny::testServer(app_server, {
    session$setInputs(selected_dataset = "NDVI", ndvi_mode = "single", ndvi_year = 2024, ndvi_month = 13)
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    session$flushReact()
    expect_equal(nrow(overview()), 0)

    session$setInputs(ndvi_month = NA)
    session$setInputs(add_dataset = 2)
    session$flushReact()
    expect_equal(nrow(overview()), 0)
  })
})

test_that("the same dataset is not added twice for the same polygon", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t")
  sel <- selected_polygon()

  shiny::testServer(app_server, {
    session$setInputs(selected_dataset = "Land Use")
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    session$flushReact()
    session$setInputs(add_dataset = 2)
    session$flushReact()
    expect_equal(nrow(overview()), 1)
    expect_equal(overview()$year, 2024L)
    expect_equal(overview()$dataset, "Land Use")
  })
})

test_that("Land Use uses the selected year, or the default until the year input exists", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t")
  sel <- selected_polygon()

  shiny::testServer(app_server, {
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2023")
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    session$flushReact()
    expect_equal(overview()$year, 2023L)
  })
})

test_that("NDVI months before the first observations are clipped or rejected", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t")
  sel <- selected_polygon()

  shiny::testServer(app_server, {
    session$setInputs(selected_dataset = "NDVI", ndvi_mode = "range",
                      ndvi_from_year = 2015, ndvi_from_month = 1, ndvi_to_year = 2016, ndvi_to_month = 12)
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)  # the whole period is before May 2017
    session$flushReact()
    expect_equal(nrow(overview()), 0)

    session$setInputs(ndvi_to_month = 12, ndvi_to_year = 2017)
    session$setInputs(add_dataset = 2)  # starts before, ends after: clipped to May 2017
    session$flushReact()
    expect_equal(overview()$date_from, as.Date("2017-05-01"))
    expect_equal(overview()$date_to, as.Date("2017-12-31"))
  })
})

# ---- long retrievals: warn, or refuse, when a dataset is added ----

ndvi_rasters <- function(session, from_year, from_month, to_year, to_month, view = "Geodata") {
  session$setInputs(selected_dataset = "NDVI", ndvi_tab = view, ndvi_mode = "range",
                    ndvi_from_year = from_year, ndvi_from_month = from_month,
                    ndvi_to_year = to_year, ndvi_to_month = to_month)
}

test_that("an NDVI raster period that would take too long is refused", {
  rec <- new.env()
  local_mocked_bindings(too_long_retrieval_message = function(seconds) { rec$refused <- seconds; "too long" })
  sel <- selected_polygon()

  with_server({
    ndvi_rasters(session, 2017, 5, 2024, 12)  # about 2,800 daily downloads
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    expect_equal(nrow(overview()), 0)
    expect_gt(rec$refused, max_request_seconds())
  })
})

test_that("a long but allowed retrieval is added, with a warning", {
  rec <- new.env()
  local_mocked_bindings(long_retrieval_warning = function(seconds) { rec$warned <- seconds; "long" },
                        too_long_retrieval_message = function(seconds) { rec$refused <- seconds; "too long" })
  sel <- selected_polygon()

  with_server({
    ndvi_rasters(session, 2018, 1, 2019, 12)  # 730 days
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    expect_equal(nrow(overview()), 1)
    expect_equal(rec$warned, 730 * request_seconds[["ndvi_day"]])
    expect_null(rec$refused)
  })
})

test_that("the whole overview counts, not only the dataset being added", {
  rec <- new.env()
  local_mocked_bindings(too_long_retrieval_message = function(seconds) { rec$refused <- seconds; "too long" })
  sel <- selected_polygon()

  with_server({
    ndvi_rasters(session, 2018, 1, 2019, 12)  # 182 seconds
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    expect_equal(nrow(overview()), 1)

    ndvi_rasters(session, 2020, 1, 2021, 6)  # 137 seconds: allowed alone, not on top of the first
    session$setInputs(add_dataset = 2)
    expect_equal(nrow(overview()), 1)
    expect_equal(rec$refused, (730 + 547) * request_seconds[["ndvi_day"]])

    # quick datasets still fit
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    session$setInputs(add_dataset = 3)
    expect_equal(nrow(overview()), 2)
  })
})

test_that("each selected polygon is retrieved separately, so it counts too", {
  rec <- new.env()
  local_mocked_bindings(too_long_retrieval_message = function(seconds) { rec$refused <- seconds; "too long" })
  both <- rbind(selected_polygon("A"), selected_polygon("B"))
  both$wkt <- c(wkt_square, sub("5\\.75", "6.75", wkt_square))  # different polygons

  with_server({
    ndvi_rasters(session, 2018, 1, 2019, 12)  # 182 seconds for one polygon, 365 for two
    selected_polygons(both)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    expect_equal(nrow(overview()), 0)
    expect_equal(rec$refused, 2 * 730 * request_seconds[["ndvi_day"]])
  })
})

test_that("the limit can be raised, and does not apply to what is quick", {
  withr::local_options(rNDC.Shiny.max_request_seconds = 1000)
  sel <- selected_polygon()

  with_server({
    ndvi_rasters(session, 2017, 5, 2024, 12)
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    expect_equal(nrow(overview()), 1)  # about 700 seconds, below the raised limit
  })

  withr::local_options(rNDC.Shiny.max_request_seconds = 300)
  with_server({
    ndvi_rasters(session, 2017, 5, 2024, 12, view = "Statistics")  # one STAC request: no limit
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    expect_equal(nrow(overview()), 1)
  })
})

test_that("the longest Weather period is allowed, with a warning", {
  rec <- new.env()
  local_mocked_bindings(long_retrieval_warning = function(seconds) { rec$warned <- seconds; "long" })
  sel <- selected_polygon()

  with_server({
    session$setInputs(selected_dataset = "Weather", weather_mode = "period",
                      weather_period = c(weather_min_date, Sys.Date()))
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    expect_equal(nrow(overview()), 1)
    expect_gt(rec$warned, warn_request_seconds)
  })
})

# ---- upload size ----

test_that("the upload limit is raised from Shiny's 5 MB, can be set, and a limit that was set is kept", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t", SHINY_APP_BASE_URL = NA, NDC_MAX_UPLOAD_MB = NA)

  withr::local_options(shiny.maxRequestSize = NULL)
  ndc_setup()
  expect_equal(getOption("shiny.maxRequestSize"), default_upload_mb * 1024^2)
  expect_equal(max_upload_mb(), 100)

  withr::local_options(shiny.maxRequestSize = 20 * 1024^2)  # set by the user
  ndc_setup()
  expect_equal(max_upload_mb(), 20)

  withr::local_envvar(NDC_MAX_UPLOAD_MB = "250")  # the variable wins
  ndc_setup()
  expect_equal(max_upload_mb(), 250)

  for (bad in c("abc", "-1", "0", "")) {
    withr::local_options(shiny.maxRequestSize = NULL)
    withr::local_envvar(NDC_MAX_UPLOAD_MB = bad)
    ndc_setup()
    expect_equal(max_upload_mb(), 100, info = bad)
  }
})

test_that("the page tells how large an upload can be", {
  withr::local_options(shiny.maxRequestSize = 42 * 1024^2)
  expect_match(as.character(app_ui()), "can be up to 42 MB", fixed = TRUE)
})
