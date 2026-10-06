# Project layers (LTER, SNL) and NDVI statistics, with the STAC API stubbed.

lter_items <- function() {
  list(stac_feature("1", list(name = "Lantaarnpaal 1", ndc_id = "101")),
       stac_feature("2", list(name = "Loobos", ndc_id = "102")),
       stac_feature("3", list(name = "Nestkast 3", ndc_id = "103")),
       stac_feature("4", list(name = "Something else", ndc_id = "104")))
}

test_that("fetch_lter_classified returns the classified collection in EPSG:4326", {
  local_stac_api(list(lter_items()))
  res <- fetch_lter_classified()
  expect_null(res$error)
  expect_s3_class(res$data, "sf")
  expect_equal(res$data$project_class, c("Light on Nature", "Loobos", "Nestboxes", NA))
  expect_equal(sf::st_crs(res$data), sf::st_crs(4326))
})

test_that("fetch_lter_classified returns the error message when the request fails", {
  local_stac_api(list(list()), status = 500L)
  res <- fetch_lter_classified()
  expect_null(res$data)
  expect_true(nzchar(res$error))
})

test_that("fetch_lter_classified returns no data and no error for an empty collection", {
  local_stac_api(list(list()))
  expect_equal(fetch_lter_classified(), list(data = NULL, error = NULL))
})

test_that("get_lter_data fetches once and then serves the cache; failures are not cached", {
  local_clean_cache()
  local_stac_api(list(lter_items()))
  expect_equal(nrow(get_lter_data()$data), 4)

  webmockr::stub_registry_clear()  # any new request would now fail
  expect_equal(nrow(get_lter_data()$data), 4)

  local_clean_cache()
  failed <- get_lter_data()
  expect_null(failed$data)
  expect_true(nzchar(failed$error))
})

test_that("fetch_snl_bbox returns the parcels, an empty status, or the error", {
  local_stac_api(list(list(stac_feature("1", list(ndc_id = "1")), stac_feature("2", list(ndc_id = "2")))))
  ok <- fetch_snl_bbox(c(5.75, 52.05, 5.76, 52.06))
  expect_equal(ok$status, "ok")
  expect_equal(nrow(ok$data), 2)
  expect_equal(last_request_body()$intersects$type, "Polygon")
  expect_equal(unlist(last_request_body()$collections), "snl")

  local_stac_api(list(list()))
  expect_equal(fetch_snl_bbox(c(5.75, 52.05, 5.76, 52.06))$status, "empty")

  local_stac_api(list(list()), status = 500L)
  err <- fetch_snl_bbox(c(5.75, 52.05, 5.76, 52.06))
  expect_equal(err$status, "error")
  expect_true(nzchar(err$error))
})

test_that("fetch_snl_bbox rejects an invalid bounding box without a request", {
  expect_equal(fetch_snl_bbox(NULL)$status, "error")
  expect_equal(fetch_snl_bbox(c(1, 2, 3))$status, "error")
  expect_equal(fetch_snl_bbox(c(1, 2, 3, NA))$status, "error")
})

ndvi_item <- function(id, ndc_id, date, mean, std = NULL) {
  props <- list(ndc_id = ndc_id, observation_date = paste0(date, "T00:00:00Z"), ndvi_mean = mean)
  if (!is.null(std)) props$ndvi_std <- std
  stac_feature(id, props)
}

ndvi_items <- function() {
  list(ndvi_item("a", "1", "2024-05-01", 0.5, 0.1), ndvi_item("b", "1", "2024-05-14", 0.7, 0.3),
       ndvi_item("c", "1", "2024-06-02", 0.8, 0.2),
       ndvi_item("d", "2", "2024-05-02", 0.9, 0.5))  # a neighbouring feature
}

test_that("NDVI statistics are aggregated per month for the selected feature only", {
  local_stac_api(list(ndvi_items()))
  res <- fetch_ndvi_stats_monthly(selected_polygon(ndc_id = "1"), "ndvi-lter",
                                  as.Date("2024-05-01"), as.Date("2024-06-30"))
  expect_equal(res$status, "ok")
  expect_equal(res$data$month, c("2024-05", "2024-06"))
  expect_equal(res$data$ndvi_mean, c(0.6, 0.8))
  expect_equal(res$data$ndvi_std, c(0.2, 0.2))
  expect_false("ndc_id" %in% names(res$data))
})

test_that("NDVI statistics keep neighbouring features when the polygon has no ndc_id", {
  local_stac_api(list(ndvi_items()))
  res <- fetch_ndvi_stats_monthly(selected_polygon(), "ndvi-lter", as.Date("2024-05-01"), as.Date("2024-06-30"))
  expect_equal(nrow(res$data), 3)  # one row per feature and month: not distinguishable
})

test_that("NDVI statistics are empty when the feature has no observations", {
  local_stac_api(list(ndvi_items()))
  res <- fetch_ndvi_stats_monthly(selected_polygon(ndc_id = "999"), "ndvi-lter",
                                  as.Date("2024-05-01"), as.Date("2024-06-30"))
  expect_equal(res$status, "empty")
})

test_that("the NDVI time range covers the whole last day", {
  local_stac_api(list(ndvi_items()))
  fetch_ndvi_stats_monthly(selected_polygon(ndc_id = "1"), "ndvi-lter", as.Date("2024-05-01"), as.Date("2024-05-31"))
  expect_equal(last_request_body()$datetime, "2024-05-01T00:00:00Z/2024-05-31T23:59:59Z")
})

test_that("NDVI statistics are skipped for areas without an NDVI collection", {
  expect_equal(fetch_ndvi_stats_monthly(selected_polygon(), NA_character_, NULL, NULL)$status, "skip")
  expect_equal(fetch_ndvi_stats_monthly(NULL, "ndvi-lter", NULL, NULL)$status, "error")
})

test_that("NDVI statistics report API errors and unexpected fields", {
  local_stac_api(list(list()), status = 500L)
  expect_equal(fetch_ndvi_stats_monthly(selected_polygon(ndc_id = "1"), "ndvi-lter", NULL, NULL)$status, "error")

  local_stac_api(list(list(stac_feature("a", list(ndc_id = "1", ndvi_mean = 0.5)))))
  res <- fetch_ndvi_stats_monthly(selected_polygon(ndc_id = "1"), "ndvi-lter", NULL, NULL)
  expect_equal(res$status, "error")
  expect_match(res$error, "observation_date")
})

test_that("the standard deviation is left out when the collection does not provide it", {
  local_stac_api(list(list(ndvi_item("a", "1", "2024-05-01", 0.5), ndvi_item("b", "1", "2024-05-02", 0.7))))
  res <- fetch_ndvi_stats_monthly(selected_polygon(ndc_id = "1"), "ndvi-lter", NULL, NULL)
  expect_equal(names(res$data), c("month", "ndvi_mean"))
  expect_equal(res$data$ndvi_mean, 0.6)
})
