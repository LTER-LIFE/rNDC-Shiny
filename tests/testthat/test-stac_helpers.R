# STAC and AgroDataCube helpers, with the HTTP requests stubbed.

test_that("ndc_get_all_sf returns the items of ALL result pages as sf", {
  local_stac_api(list(list(stac_feature("a"), stac_feature("b")), list(stac_feature("c"))))
  out <- ndc_get_all_sf("lter")
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 3)
  expect_equal(sum(grepl("/search$", request_uris())), 2)  # one request per page
})

test_that("ndc_get_all_sf sends the collection, the RoI and the time range", {
  local_stac_api(list(list(stac_feature("a"))))
  ndc_get_all_sf("ndvi-lter", roi = selected_polygon(), trange = "2024-05-01T00:00:00Z/2024-05-31T23:59:59Z")
  body <- last_request_body()
  expect_equal(unlist(body$collections), "ndvi-lter")
  expect_equal(body$intersects$type, "Polygon")
  expect_equal(body$datetime, "2024-05-01T00:00:00Z/2024-05-31T23:59:59Z")
})

test_that("ndc_get_all_sf returns NULL when nothing matches", {
  local_stac_api(list(list()))
  expect_null(ndc_get_all_sf("lter"))
})

test_that("ndc_get_all_sf passes API errors on", {
  local_stac_api(list(list()), status = 500L)
  expect_error(ndc_get_all_sf("lter"))
})

fields_stub <- function(...) {
  stub <- webmockr::stub_request("get", uri_regex = adc_re("fields"))
  for (page in list(...)) {
    stub <- webmockr::to_return(stub, body = json_body(page), headers = json_header)
  }
  invisible(stub)
}

test_that("adc_get_all combines the pages until one comes back short", {
  local_webmock()
  fields_stub(geojson_features(1:2), geojson_features(3:4), geojson_features(5))
  res <- adc_get_all("Fields", c(geometry = "g", epsg = "4326"), token = "t", page_size = 2)
  expect_length(res$features, 5)
  expect_equal(vapply(res$features, function(f) f$properties$fieldid, numeric(1)), 1:5)

  uris <- request_uris()
  expect_length(uris, 3)
  expect_match(uris, "page_size=2", all = TRUE)
  for (i in 0:2) expect_match(uris[i + 1], paste0("page_offset=", i, "($|&)"))
})

test_that("adc_get_all stops after an exact multiple of the page size", {
  local_webmock()
  fields_stub(geojson_features(1:2), geojson_features(3:4), geojson_features(integer()))
  res <- adc_get_all("Fields", c(geometry = "g"), token = "t", page_size = 2)
  expect_length(res$features, 4)
  expect_length(request_uris(), 3)
})

test_that("adc_get_all makes a single request when the first page is short", {
  local_webmock()
  fields_stub(geojson_features(1:3))
  expect_length(adc_get_all("Fields", c(geometry = "g"), token = "t")$features, 3)
  expect_length(request_uris(), 1)
})

test_that("adc_get_all warns when it hits the page limit", {
  local_webmock()
  fields_stub(geojson_features(1:2))  # the last stubbed response repeats
  expect_warning(res <- adc_get_all("Fields", c(geometry = "g"), token = "t", page_size = 2, max_pages = 3),
                 "stopped after 3 pages")
  expect_length(res$features, 6)
})

test_that("adc_get_all reports HTTP errors with the message of the server", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("fields")) |>
    webmockr::to_return(body = json_body(list(status = "Geometry area too large")), status = 403,
                        headers = json_header)
  expect_error(adc_get_all("Fields", c(geometry = "g"), token = "t"), "HTTP 403.*Geometry area too large")
})

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
