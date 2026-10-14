# ndc_setup() and ndc_app(): the credentials, the options, and the page. (R/ndc_app.R)

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
