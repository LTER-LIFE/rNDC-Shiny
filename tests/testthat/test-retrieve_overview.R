# Retrieving a whole overview without Shiny, and packing the result for a background process. (R/retrieve_overview.R)

test_that("rasters and vectors survive being sent to another process when they are packed", {
  r <- raster()
  v <- terra::vect(sf::st_sf(a = 1, geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326)))

  roundtrip <- function(x) {
    f <- withr::local_tempfile()
    saveRDS(x, f)
    readRDS(f)
  }
  packed <- roundtrip(retrieve_pack(r))
  expect_s4_class(packed, "PackedSpatRaster")
  expect_equal(terra::values(retrieve_unpack(packed))[, 1], 1:4)
  expect_equal(nrow(retrieve_unpack(roundtrip(retrieve_pack(v)))), 1)

  # what is not a terra object is left alone
  df <- data.frame(a = 1)
  expect_identical(retrieve_pack(df), df)
  expect_identical(retrieve_unpack(df), df)
})

test_that("retrieve_and_package returns the data, or not, and wraps rasters when asked", {
  local_stac_api()  # the references are read from the STAC API
  local_mocked_bindings(ndc_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  ov <- overview_row("Land Use", year = 2024L)
  calls <- list()
  progress <- function(detail = NULL, value = NULL) calls[[length(calls) + 1]] <<- list(detail = detail, value = value)

  res <- retrieve_and_package(ov, save_files = FALSE, ndc_token = "t", adc_token = "t", progress = progress)
  expect_s4_class(res$datasets$`Land Use_1`, "SpatRaster")
  expect_true(res$produced_any)
  expect_equal(res$messages, "Retrieved: Land Use raster for year 2024")
  expect_equal(calls[[1]], list(detail = "Land Use (Own polygon)", value = 0))
  expect_equal(calls[[length(calls)]], list(detail = NULL, value = 1))

  packed <- retrieve_and_package(ov, save_files = FALSE, ndc_token = "t", adc_token = "t", pack = TRUE)
  expect_s4_class(packed$datasets$`Land Use_1`, "PackedSpatRaster")

  none <- retrieve_and_package(ov, save_files = FALSE, ndc_token = "t", adc_token = "t", return_data = FALSE)
  expect_length(none$datasets, 0)
  expect_true(none$produced_any)
})

test_that("retrieve_and_package zips, and leaves nothing behind", {
  local_stac_api()  # the references are read from the STAC API
  local_mocked_bindings(ndc_landuse_raster = function(...) list(stack = raster()), .package = "rNDC")
  before <- list.files(tempdir(), "^ndc_export_")
  zip <- withr::local_tempfile(fileext = ".zip")

  res <- retrieve_and_package(overview_row("Land Use", year = 2024L), zipfile = zip, ndc_token = "t", adc_token = "t")
  expect_true(file.exists(zip))
  expect_null(res$out_dir)
  expect_equal(res$zipfile, zip)
  expect_identical(list.files(tempdir(), "^ndc_export_"), before)
})
