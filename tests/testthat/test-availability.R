# "Is there data for the selected area?": counts of STAC items per collection, and the hint in the app.

test_that("count_items returns the number of matching items, asks for the area and the period, and caches", {
  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"), raster_item("b", "2024-01-01"))))

  n <- count_items("lgn", sf::st_geometry(selected_polygon()), "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z")
  expect_equal(n, 2)
  body <- last_request_body()
  expect_equal(unlist(body$collections), "lgn")
  expect_equal(body$intersects$type, "Polygon")
  expect_equal(body$datetime, "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z")

  n_requests <- length(request_uris())
  expect_equal(count_items("lgn", sf::st_geometry(selected_polygon()), "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z"), 2)
  expect_length(request_uris(), n_requests)  # from the cache

  # another period is another question
  local_stac_api(list(list()))
  expect_equal(count_items("lgn", sf::st_geometry(selected_polygon()), "2025-01-01T00:00:00Z/2025-12-31T23:59:59Z"), 0)
})

test_that("count_items gives NA, and does not cache, when the request fails", {
  local_clean_cache()
  local_stac_api(list(list()), status = 500L)
  expect_true(is.na(count_items("lgn", sf::st_geometry(selected_polygon()))))
  expect_length(ls(ndc_cache), 0)
})

test_that("items_in_area counts per collection for a year", {
  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"))))
  counts <- items_in_area(c("ntot", "nox"), sf::st_geometry(selected_polygon()), year = "2024")
  expect_equal(counts, c(ntot = 1, nox = 1))
  expect_equal(last_request_body()$datetime, "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z")

  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"))))
  items_in_area("ntot", sf::st_geometry(selected_polygon()))  # no year: no period
  expect_null(last_request_body()$datetime)
})

test_that("the app says whether the selected area has data for the selected raster dataset", {
  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"))))
  sel <- selected_polygon()

  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    selected_polygons(sel)
    session$flushReact()
    session$elapse(500)
    expect_match(as.character(output$availability$html), "Land Use data for 2024 is available for the selected area.",
                 fixed = TRUE)
  })
})

test_that("the app warns when there is no data for the area, naming the missing nitrogen layers", {
  local_clean_cache()
  local_stac_api(list(list()))  # every collection: no items
  sel <- selected_polygon()

  with_server({
    session$setInputs(selected_dataset = "Nitrogen", nitrogen_year = "2025")
    selected_polygons(sel)
    session$flushReact()
    session$elapse(500)
    html <- as.character(output$availability$html)
    expect_match(html, "No Nitrogen data for 2025 found for the selected area (ntot, nox, nh3).", fixed = TRUE)
  })
})

test_that("nothing is shown for other datasets, without a selection, or when the check fails", {
  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"))))
  sel <- selected_polygon()

  with_server({
    session$elapse(500)
    expect_null(output$availability$html)  # Land Use, but nothing selected

    selected_polygons(sel)
    session$setInputs(selected_dataset = "AHN")
    session$flushReact()
    session$elapse(500)
    expect_null(output$availability$html)
  })

  local_clean_cache()
  local_stac_api(list(list()), status = 500L)
  with_server({
    selected_polygons(sel)
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    session$flushReact()
    session$elapse(500)
    expect_null(output$availability$html)  # unknown: say nothing
  })
})

test_that("several selected polygons are asked as one area", {
  local_clean_cache()
  local_stac_api(list(list(raster_item("a", "2024-01-01"))))
  a <- selected_polygon("A")
  b <- selected_polygon("B")
  b$geometry <- sf::st_geometry(fixed_square(1, 6.1, 52.1))
  both <- rbind(a, b)

  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    selected_polygons(both)
    session$flushReact()
    session$elapse(500)
    expect_equal(last_request_body()$intersects$type, "MultiPolygon")
  })
})
