# Helpers for polygons: WKT columns, names, ids and drawn polygons, and the limits on polygons. (R/polygon_utils.R)

test_that("polygon_add_wkt adds the WKT in EPSG:4326", {
  x <- sf::st_transform(selected_polygon()[, "geometry"], 28992)
  out <- polygon_add_wkt(x)
  expect_equal(sf::st_crs(out), sf::st_crs(4326))
  expect_match(out$wkt, "^POLYGON")
  expect_equal(sf::st_as_sfc(out$wkt, crs = 4326), sf::st_geometry(out), ignore_attr = TRUE, tolerance = 1e-6)
})

test_that("polygon_assign_names continues after the names already in use", {
  two <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(1, 1)), sf::st_point(c(2, 2))))
  none <- NULL
  first <- polygon_assign_names(two, "Nestboxes", none, none, none, none)
  expect_equal(first$source_name, c("Nestboxes_1", "Nestboxes_2"))

  overview_df <- data.frame(polygon = c("Nestboxes_1", "Nestboxes_5", "Loobos_9"))
  later <- polygon_assign_names(two, "Nestboxes", overview_df, none, none, none)
  expect_equal(later$source_name, c("Nestboxes_6", "Nestboxes_7"))

  drawn <- data.frame(source_name = "Nestboxes_2")
  expect_equal(polygon_assign_names(two, "Nestboxes", none, none, none, drawn)$source_name,
               c("Nestboxes_3", "Nestboxes_4"))

  expect_null(polygon_assign_names(NULL, "x", none, none, none, none))
})

test_that("geometries drawn on the map are converted", {
  feat <- list(geometry = list(type = "Polygon",
                               coordinates = list(list(list(5, 52), list(5.1, 52), list(5.1, 52.1), list(5, 52)))))
  out <- polygon_from_drawing(feat, start_layer_id = 7)
  expect_s3_class(out, "sf")
  expect_equal(out$layer_id, 7L)
  expect_equal(sf::st_crs(out), sf::st_crs(4326))
  expect_null(polygon_from_drawing(list(geometry = list(type = "Point")), 1))
  expect_null(polygon_from_drawing(NULL, 1))

  clicked <- feat
  clicked$properties <- list(layerId = 3)
  expect_equal(polygon_from_geojson(clicked)$layer_id, 3L)
  expect_true(is.na(polygon_from_geojson(feat)$layer_id))
})

test_that("polygon_next_id is one more than the largest id of the layers, whatever is missing", {
  a <- sf::st_sf(layer_id = c(1L, 4L), geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326))
  b <- sf::st_sf(layer_id = 7L, geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
  expect_equal(polygon_next_id(), 1L)
  expect_equal(polygon_next_id(NULL, NULL), 1L)
  expect_equal(polygon_next_id(a, NULL), 5L)
  expect_equal(polygon_next_id(a, b, NULL), 8L)
  expect_equal(polygon_next_id(a[0, ]), 1L)
})

# ---- limits ----


square_at <- function(x, y, size) {
  geom <- sf::st_as_sfc(sf::st_bbox(c(xmin = x, ymin = y, xmax = x + size, ymax = y + size), crs = sf::st_crs(4326)))
  sf::st_sf(wkt = sf::st_as_text(geom), source_name = "Own polygon", layer_id = 1L, geometry = geom)
}

test_that("the area of a polygon is measured in km2", {
  # 0.01 degrees at 52 N: about 0.68 km x 1.11 km
  area <- polygon_area_km2(square_at(5, 52, 0.01))
  expect_gt(area, 0.6)
  expect_lt(area, 0.9)
  expect_equal(polygon_area_km2(square_at(5, 52, 0.1)), 100 * area, tolerance = 0.02)
})

test_that("polygons above the limit are named", {
  sel <- rbind(square_at(5, 52, 0.01), transform(square_at(5, 52, 1), source_name = "Big"))
  expect_equal(polygon_too_large(sel, limit = 1000), "Big")
  expect_equal(polygon_too_large(sel, limit = 0.1), c("Own polygon", "Big"))
  expect_equal(polygon_too_large(sel, limit = 1e6), character(0))
  expect_equal(polygon_too_large(NULL), character(0))
  expect_match(polygon_too_large_message("Big", 1000), "the polygon Big is larger than the limit of 1,000 km")
  expect_match(polygon_too_large_message(c("A", "B"), 5), "the polygons A, B are larger")
})

test_that("NDC_MAX_AREA_KM2 sets the limit; nonsense is ignored", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t", SHINY_APP_BASE_URL = NA)
  withr::local_options(rNDC.Shiny.max_area_km2 = NULL)
  expect_equal(max_area_km2(), 1000)

  withr::local_envvar(NDC_MAX_AREA_KM2 = "250")
  ndc_setup()
  expect_equal(max_area_km2(), 250)

  options(rNDC.Shiny.max_area_km2 = NULL)
  for (bad in c("abc", "-5", "0", "")) {
    withr::local_envvar(NDC_MAX_AREA_KM2 = bad)
    ndc_setup()
    expect_equal(max_area_km2(), 1000, info = bad)
  }
})

test_that("a polygon above the limit is not added, except for Weather, and the others still are", {
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))
  big <- transform(square_at(5, 52, 1), source_name = "Big", layer_id = 2L)
  sel <- rbind(square_at(5.5, 52.5, 0.01), big)
  sel$source <- "drawn"

  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    selected_polygons(sel)
    session$flushReact()
    session$setInputs(add_dataset = 1)
    expect_equal(overview()$polygon, "Own polygon")  # the small one only
    expect_true(any(grepl("the polygon Big is larger", rec$notes)))

    # nothing else to add: the message about the large one only
    rec$notes <- character(0)
    selected_polygons(big)
    session$flushReact()
    session$setInputs(add_dataset = 2)
    expect_equal(nrow(overview()), 1)
    expect_false(any(grepl("already present", rec$notes)))
    expect_true(any(grepl("larger than the limit", rec$notes)))

    # Weather uses the centroid: any size
    session$setInputs(selected_dataset = "Weather", weather_mode = "single", weather_date = Sys.Date() - 2)
    selected_polygons(big)
    session$flushReact()
    session$setInputs(add_dataset = 3)
    expect_equal(overview()$dataset, c("Land Use", "Weather"))
  })
})

test_that("the panels to upload and to draw are not disabled, and say what the limits are", {
  html <- as.character(app_ui())
  expect_false(grepl("disabled-category", html, fixed = TRUE))
  expect_match(html, "Upload your own polygon", fixed = TRUE)
  expect_match(html, "Draw your own polygon", fixed = TRUE)
  expect_match(html, "up to 1,000 km2", fixed = TRUE)
  expect_match(html, paste0("at most ", max_upload_features, " polygons"), fixed = TRUE)
})
