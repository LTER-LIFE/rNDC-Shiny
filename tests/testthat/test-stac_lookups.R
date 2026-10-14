# STAC and AgroDataCube helpers, with the HTTP requests stubbed.

# The retrieval of all pages (ndc_get(all_pages = TRUE), adc_get_all()) and of the years (ndc_nitrogen_years(),
# ndc_landuse_years()) is tested in rNDC: here the use that the app makes of them.

test_that("stac_nitrogen_years reads the years from the STAC items and caches them", {
  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"), raster_item("b", "2040-01-01"),
                           raster_item("c", "2025-01-01"))))
  expect_equal(stac_nitrogen_years(), c("2024", "2025", "2040"))
  n_requests <- length(request_uris())

  expect_equal(stac_nitrogen_years(), c("2024", "2025", "2040"))  # from the cache
  expect_length(request_uris(), n_requests)
})

test_that("stac_nitrogen_years falls back to the known years, without caching them", {
  local_clean_cache()
  local_stac_api(list(list()), status = 500L)
  expect_equal(stac_nitrogen_years(), c("2024", "2025", "2040"))
  expect_null(cache_get("nitrogen_years"))
})

test_that("stac_landuse_years reads the years of the land use items, and falls back to the default year", {
  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"), raster_item("b", "2023-01-01"))))
  expect_equal(stac_landuse_years(), c("2023", "2024"))
  expect_equal(cache_get("landuse_years"), c("2023", "2024"))
  expect_equal(unlist(last_request_body()$collections), ndc_landuse_collection)

  local_clean_cache()
  local_stac_api(list(list()), status = 500L)
  expect_equal(stac_landuse_years(), "2024")
  expect_null(cache_get("landuse_years"))
})

test_that("years are cached per key, and only successful reads are cached", {
  local_clean_cache()
  n <- 0
  fetch <- function() { n <<- n + 1; c("2030", "2031") }
  expect_equal(stac_raster_years("k1", fetch, "none"), c("2030", "2031"))
  expect_equal(stac_raster_years("k1", fetch, "none"), c("2030", "2031"))  # from the cache
  expect_equal(n, 1)
  expect_equal(stac_raster_years("k2", fetch, "none"), c("2030", "2031"))  # another key, another read
  expect_equal(n, 2)

  expect_equal(stac_raster_years("k3", function() stop("no network"), "none"), "none")
  expect_equal(stac_raster_years("k4", function() character(0), "none"), "none")
  expect_null(cache_get("k3"))
  expect_null(cache_get("k4"))
})

test_that("stac_count_items returns the number of matching items, asks for the area and the period, and caches", {
  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"), raster_item("b", "2024-01-01"))))

  n <- stac_count_items("lgn", sf::st_geometry(selected_polygon()), "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z")
  expect_equal(n, 2)
  body <- last_request_body()
  expect_equal(unlist(body$collections), "lgn")
  expect_equal(body$intersects$type, "Polygon")
  expect_equal(body$datetime, "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z")

  n_requests <- length(request_uris())
  expect_equal(stac_count_items("lgn", sf::st_geometry(selected_polygon()), "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z"), 2)
  expect_length(request_uris(), n_requests)  # from the cache

  # another period is another question
  local_stac_api(list(list()))
  expect_equal(stac_count_items("lgn", sf::st_geometry(selected_polygon()), "2025-01-01T00:00:00Z/2025-12-31T23:59:59Z"), 0)
})

test_that("stac_count_items gives NA, and does not cache, when the request fails", {
  local_clean_cache()
  local_stac_api(list(list()), status = 500L)
  expect_true(is.na(stac_count_items("lgn", sf::st_geometry(selected_polygon()))))
  expect_length(ls(ndc_cache), 0)
})

test_that("the token is passed on to the counts and the years", {
  local_clean_cache()
  seen <- character()
  local_mocked_bindings(ndc_count = function(..., token) { seen <<- c(seen, token); 1 }, .package = "rNDC")
  expect_equal(stac_count_items("lgn", sf::st_geometry(selected_polygon()), token = "mine"), 1)
  expect_equal(seen, "mine")

  local_mocked_bindings(ndc_landuse_years = function(token) { seen <<- c(seen, token); "2024" }, .package = "rNDC")
  expect_equal(stac_landuse_years("mine2"), "2024")
  expect_equal(seen, c("mine", "mine2"))
})
