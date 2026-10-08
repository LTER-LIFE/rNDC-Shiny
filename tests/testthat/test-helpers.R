# Pure helpers: no network access.

test_that("dataset and tab helpers", {
  expect_equal(make_tab_id("Land Use"), "land_use_tab")
  expect_equal(make_target_id("Soil map", "Geodata"), "tab_target_soil_map_geodata")
  expect_equal(tabs_for_dataset("NDVI"), c("Statistics", "Geodata"))
  expect_equal(tabs_for_dataset("Weather"), "Statistics")
  expect_equal(tabs_for_dataset("Land Use"), "Geodata")
  expect_equal(default_tab_for_dataset("NDVI"), "Statistics")
  expect_true(all(adc_datasets %in% all_dataset_names))
})

test_that("rNDC constants are available", {
  expect_equal(landuse_default_year, 2024L)
  expect_equal(nitrogen_layer_choices, c("ntot", "nox", "nh3"))
})

test_that("safe_filename and same_na", {
  expect_equal(safe_filename("a b/c.d"), "a_b_c_d")
  expect_equal(safe_filename("Light on Nature_1"), "Light_on_Nature_1")
  expect_true(same_na(NA, NA))
  expect_true(same_na(2024L, 2024L))
  expect_false(same_na(NA, 2024L))
  expect_false(same_na(2024L, 2025L))
})

test_that("classify_lter assigns the four project classes", {
  x <- sf::st_sf(name = c("Lantaarnpaal 12", "Loobos", "Nestkast 3", "Nutnet", "Something else"),
                 geometry = sf::st_sfc(rep(list(sf::st_point(c(5, 52))), 5), crs = 4326))
  expect_equal(classify_lter(x)$project_class,
               c("Light on Nature", "Loobos", "Nestboxes", "Nutnet", NA))
  # Without a name column, or empty: unchanged
  expect_identical(classify_lter(x[, "geometry"]), x[, "geometry"])
  expect_null(classify_lter(NULL))
})

test_that("detect_ndvi_collection maps source names to NDVI collections", {
  expect_equal(detect_ndvi_collection("SNL parcel_12"), "ndvi-snl")
  expect_equal(detect_ndvi_collection("SNL parcel"), "ndvi-snl")
  expect_equal(detect_ndvi_collection("Nestboxes_3"), "ndvi-lter")
  expect_equal(detect_ndvi_collection("Light on Nature_1"), "ndvi-lter")
  expect_equal(detect_ndvi_collection("Loobos_1"), "ndvi-lter")
  expect_true(is.na(detect_ndvi_collection("Own polygon")))
  expect_true(is.na(detect_ndvi_collection("Loobos.gpkg")))
  expect_true(is.na(detect_ndvi_collection("")))
  expect_true(is.na(detect_ndvi_collection(NULL)))
  expect_true(is.na(detect_ndvi_collection(NA_character_)))
})

test_that("add_wkt_column adds the WKT in EPSG:4326", {
  x <- sf::st_transform(selected_polygon()[, "geometry"], 28992)
  out <- add_wkt_column(x)
  expect_equal(sf::st_crs(out), sf::st_crs(4326))
  expect_match(out$wkt, "^POLYGON")
  expect_equal(sf::st_as_sfc(out$wkt, crs = 4326), sf::st_geometry(out), ignore_attr = TRUE, tolerance = 1e-6)
})

test_that("assign_sequential_source_names continues after the names already in use", {
  two <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(1, 1)), sf::st_point(c(2, 2))))
  none <- NULL
  first <- assign_sequential_source_names(two, "Nestboxes", none, none, none, none)
  expect_equal(first$source_name, c("Nestboxes_1", "Nestboxes_2"))

  overview_df <- data.frame(polygon = c("Nestboxes_1", "Nestboxes_5", "Loobos_9"))
  later <- assign_sequential_source_names(two, "Nestboxes", overview_df, none, none, none)
  expect_equal(later$source_name, c("Nestboxes_6", "Nestboxes_7"))

  drawn <- data.frame(source_name = "Nestboxes_2")
  expect_equal(assign_sequential_source_names(two, "Nestboxes", none, none, none, drawn)$source_name,
               c("Nestboxes_3", "Nestboxes_4"))

  expect_null(assign_sequential_source_names(NULL, "x", none, none, none, none))
})

test_that("geometries drawn on the map are converted", {
  feat <- list(geometry = list(type = "Polygon",
                               coordinates = list(list(list(5, 52), list(5.1, 52), list(5.1, 52.1), list(5, 52)))))
  out <- convert_drawn_to_sf(feat, start_layer_id = 7)
  expect_s3_class(out, "sf")
  expect_equal(out$layer_id, 7L)
  expect_equal(sf::st_crs(out), sf::st_crs(4326))
  expect_null(convert_drawn_to_sf(list(geometry = list(type = "Point")), 1))
  expect_null(convert_drawn_to_sf(NULL, 1))

  clicked <- feat
  clicked$properties <- list(layerId = 3)
  expect_equal(convert_geojson_feature_to_sf(clicked)$layer_id, 3L)
  expect_true(is.na(convert_geojson_feature_to_sf(feat)$layer_id))
})

test_that("read_polygons_from_path keeps polygons, assumes EPSG:4326 and drops other geometries", {
  skip_if_not_installed("sf")
  path <- withr::local_tempfile(fileext = ".geojson")
  sf::st_write(rbind(selected_polygon()[, "geometry"],
                     sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326))),
               path, quiet = TRUE)
  out <- read_polygons_from_path(path)
  expect_equal(nrow(out), 1)
  expect_equal(sf::st_crs(out), sf::st_crs(4326))
  expect_null(read_polygons_from_path(withr::local_tempfile(fileext = ".gpkg")))  # does not exist
})

test_that("uploaded GeoPackages are imported, or queued when they have several layers", {
  single <- withr::local_tempfile(fileext = ".gpkg")
  sf::st_write(selected_polygon()[, "geometry"], single, layer = "a", quiet = TRUE)
  res <- process_uploaded_files(data.frame(name = "single.gpkg", datapath = single))
  expect_equal(nrow(res$imported), 1)
  expect_equal(res$imported$source_name, "single.gpkg")
  expect_length(res$pending_gpkg, 0)

  multi <- withr::local_tempfile(fileext = ".gpkg")
  sf::st_write(selected_polygon()[, "geometry"], multi, layer = "a", quiet = TRUE)
  sf::st_write(selected_polygon()[, "geometry"], multi, layer = "b", quiet = TRUE)
  res <- process_uploaded_files(data.frame(name = "multi.gpkg", datapath = multi))
  expect_null(res$imported)
  expect_equal(res$pending_gpkg[[1]]$layers, c("a", "b"))

  expect_equal(process_uploaded_files(NULL), list(imported = NULL, pending_gpkg = NULL))
})

test_that("the cache returns values until they expire", {
  local_clean_cache()
  expect_null(cache_get("k"))
  cache_set("k", 42)
  expect_equal(cache_get("k"), 42)
  cache_set("none", NULL)  # NULL is never cached
  expect_null(cache_get("none"))

  local_mocked_bindings(ndc_cache_ttl = -1)
  expect_null(cache_get("k"))
})

test_that("the cache drops expired entries and keeps at most the maximum number, oldest first", {
  local_clean_cache()
  local_mocked_bindings(ndc_cache_max_entries = 3L)
  for (k in c("a", "b", "c", "d")) {
    cache_set(k, k)
    Sys.sleep(0.01)  # distinct times
  }
  expect_setequal(ls(ndc_cache), c("b", "c", "d"))  # "a" was the oldest

  # expired entries go when something is added, also when the maximum is not reached
  local_mocked_bindings(ndc_cache_ttl = -1)
  cache_set("e", "e")
  expect_length(ls(ndc_cache), 0)
})

test_that("next_layer_id is one more than the largest id of the layers, whatever is missing", {
  a <- sf::st_sf(layer_id = c(1L, 4L), geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326))
  b <- sf::st_sf(layer_id = 7L, geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
  expect_equal(next_layer_id(), 1L)
  expect_equal(next_layer_id(NULL, NULL), 1L)
  expect_equal(next_layer_id(a, NULL), 5L)
  expect_equal(next_layer_id(a, b, NULL), 8L)
  expect_equal(next_layer_id(a[0, ]), 1L)
})

test_that("the LTER classes come from one list: the first matching pattern wins and the menu follows it", {
  expect_equal(names(lter_class_patterns), lter_class_levels)
  x <- sf::st_sf(name = c("Lantaarnpaal 3", "LOOBOS", "Nestkast Veluwe", "Nutnet", "Other", NA),
                 geometry = sf::st_sfc(rep(list(sf::st_point(c(0, 0))), 6), crs = 4326))
  expect_equal(classify_lter(x)$project_class,
               c("Light on Nature", "Loobos", "Nestboxes", "Nutnet", NA, NA))

  html <- as.character(app_ui())
  for (cls in lter_class_levels) expect_match(html, paste0("lter:", cls), fixed = TRUE)
  expect_equal(detect_ndvi_collection("Light on Nature_12"), "ndvi-lter")
})
