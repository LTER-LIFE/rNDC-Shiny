# Live checks against the real APIs. They are skipped by default (and on CRAN) and run with
#   RNDC_LIVE_TESTS=true NDC_TOKEN=... ADC_TOKEN=... Rscript -e 'devtools::test(filter = "live")'
# They detect changes in the APIs that the offline tests cannot see, such as renamed collections.

skip_unless_live <- function() {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("RNDC_LIVE_TESTS"), "true"), "live API tests are opt-in (RNDC_LIVE_TESTS=true)")
  skip_if(!nzchar(Sys.getenv("NDC_TOKEN")) || !nzchar(Sys.getenv("ADC_TOKEN")),
          "NDC_TOKEN and ADC_TOKEN are required")
}

test_that("the collections used by the app exist", {
  skip_unless_live()
  expect_true(all(c(ndc_lter_collection, ndc_snl_collection, "ndvi-lter", "ndvi-snl", "lgn", nitrogen_layer_choices)
                  %in% rNDC::ndc_datasets()))
})

test_that("the LTER projects are found and classified", {
  skip_unless_live()
  local_clean_cache()
  res <- get_lter_data()
  expect_null(res$error)
  expect_setequal(stats::na.omit(unique(res$data$project_class)), lter_class_levels)
  expect_true("ndc_id" %in% names(res$data))
})

test_that("SNL parcels can be fetched for a map view", {
  skip_unless_live()
  res <- fetch_snl_bbox(c(5.75, 52.05, 5.76, 52.06))
  expect_equal(res$status, "ok")
  expect_true(all(c("ndc_id", "name") %in% names(res$data)))
})

test_that("NDVI statistics belong to the selected LTER feature only", {
  skip_unless_live()
  local_clean_cache()
  lter <- get_lter_data()$data
  poly <- add_wkt_column(lter[lter$project_class == "Loobos", ][1, ])
  res <- fetch_ndvi_stats_monthly(poly, "ndvi-lter", as.Date("2024-05-01"), as.Date("2024-07-31"))
  expect_equal(res$status, "ok")
  expect_equal(anyDuplicated(res$data$month), 0)
  expect_true(all(res$data$ndvi_mean > -1 & res$data$ndvi_mean < 1))
})

test_that("the nitrogen years are available", {
  skip_unless_live()
  local_clean_cache()
  expect_true(all(c("2024", "2025") %in% get_nitrogen_years()))
})

test_that("AgroDataCube results are fetched completely", {
  skip_unless_live()
  # About 700 fields in this area: more than the default page of the API
  poly <- "POLYGON((5.70 52.00,5.85 52.00,5.85 52.10,5.70 52.10,5.70 52.00))"
  res <- adc_get_all("Fields", c(geometry = poly, epsg = "4326", year = "2024", output_epsg = "4326"),
                     token = Sys.getenv("ADC_TOKEN"))
  expect_gt(length(res$features), 100)
})
