# STAC and AgroDataCube helpers, with the HTTP requests stubbed.

# The retrieval of all pages (ndc_get(all_pages = TRUE), adc_get_all()) and of the years (ndc_nitrogen_years(),
# ndc_landuse_years()) is tested in rNDC: here the use that the app makes of them.

test_that("get_nitrogen_years reads the years from the STAC items and caches them", {
  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"), raster_item("b", "2040-01-01"),
                           raster_item("c", "2025-01-01"))))
  expect_equal(get_nitrogen_years(), c("2024", "2025", "2040"))
  n_requests <- length(request_uris())

  expect_equal(get_nitrogen_years(), c("2024", "2025", "2040"))  # from the cache
  expect_length(request_uris(), n_requests)
})

test_that("get_nitrogen_years falls back to the known years, without caching them", {
  local_clean_cache()
  local_stac_api(list(list()), status = 500L)
  expect_equal(get_nitrogen_years(), c("2024", "2025", "2040"))
  expect_null(cache_get("nitrogen_years"))
})

test_that("get_landuse_years reads the years of the land use items, and falls back to the default year", {
  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"), raster_item("b", "2023-01-01"))))
  expect_equal(get_landuse_years(), c("2023", "2024"))
  expect_equal(cache_get("landuse_years"), c("2023", "2024"))
  expect_equal(unlist(last_request_body()$collections), ndc_landuse_collection)

  local_clean_cache()
  local_stac_api(list(list()), status = 500L)
  expect_equal(get_landuse_years(), "2024")
  expect_null(cache_get("landuse_years"))
})

test_that("years are cached per key, and only successful reads are cached", {
  local_clean_cache()
  n <- 0
  fetch <- function() { n <<- n + 1; c("2030", "2031") }
  expect_equal(get_raster_years("k1", fetch, "none"), c("2030", "2031"))
  expect_equal(get_raster_years("k1", fetch, "none"), c("2030", "2031"))  # from the cache
  expect_equal(n, 1)
  expect_equal(get_raster_years("k2", fetch, "none"), c("2030", "2031"))  # another key, another read
  expect_equal(n, 2)

  expect_equal(get_raster_years("k3", function() stop("no network"), "none"), "none")
  expect_equal(get_raster_years("k4", function() character(0), "none"), "none")
  expect_null(cache_get("k3"))
  expect_null(cache_get("k4"))
})
