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

test_that("the problems of a selection are a total area above the limit and too many polygons", {
  small <- square_at(5, 52, 0.01)
  big <- transform(square_at(5, 52, 1), source_name = "Big", layer_id = 2L)
  expect_equal(selection_problems(NULL), character(0))
  expect_equal(selection_problems(small), character(0))
  expect_equal(selection_problems(rbind(small, big), limit_area = 1e5), character(0))

  # the area is the sum over the polygons, not the one of each
  one <- selection_problems(rbind(small, big))
  expect_named(one, "area")
  expect_match(one[["area"]], "^The selected polygons cover 7,\\d\\d0 km\u00b2 in total: at most 2,000 km\u00b2 can be used at once\\.$")
  halves <- do.call(rbind, lapply(1:3, function(i) transform(square_at(5 + i / 5, 52, 0.5), layer_id = i)))   # about 1,900 km2 each
  expect_gt(max(polygon_area_km2(halves)), 1500)
  expect_lt(max(polygon_area_km2(halves)), 2000)                                  # each is within the limit ...
  expect_named(selection_problems(halves), "area")                                # ... together they are not
  expect_equal(selection_problems(halves[1, ]), character(0))
  expect_match(selection_problems(small, limit_area = 0.1)[["area"]], "^The selected polygons cover 0\\.\\d+ km\u00b2 in total: at most 0\\.1 km\u00b2 can be used at once\\.$")

  many <- do.call(rbind, lapply(1:101, function(i) transform(square_at(5, 52, 0.001), layer_id = i)))
  expect_equal(selection_problems(many), c(count = "101 polygons are selected: at most 100 can be used at once."))
  expect_equal(selection_problems(many[1:100, ]), character(0))                  # (a hundred is allowed)
  expect_equal(selection_problems(many, limit_n = 200), character(0))
  both <- selection_problems(rbind(many, big))
  expect_named(both, c("count", "area"))                                          # both problems are told
  expect_match(both[["count"]], "102 polygons")
})

test_that("the area does not count for Weather, the number of polygons does", {
  problems <- c(count = "too many", area = "too large")
  expect_equal(problems_for(problems, "Land Use"), problems)
  expect_equal(problems_for(problems, "Weather"), c(count = "too many"))
  expect_equal(problems_for(c(area = "too large"), "Weather"), character(0))
  expect_equal(problems_for(c(area = "too large"), "AHN"), c(area = "too large"))
  expect_equal(problems_for(character(0), "AHN"), character(0))
})

test_that("the most polygons is 100, NDC_MAX_POLYGONS sets it, and nonsense is ignored", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t", SHINY_APP_BASE_URL = NA)
  withr::local_options(rNDC.Shiny.max_polygons = NULL)
  expect_equal(max_polygons(), 100)

  withr::local_envvar(NDC_MAX_POLYGONS = "25")
  ndc_setup()
  expect_equal(max_polygons(), 25)
  options(rNDC.Shiny.max_polygons = NULL)
  for (bad in c("abc", "-5", "0", "")) {
    withr::local_envvar(NDC_MAX_POLYGONS = bad)
    ndc_setup()
    expect_equal(max_polygons(), 100, info = bad)
  }
})

test_that("NDC_MAX_AREA_KM2 sets the limit; nonsense is ignored", {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t", SHINY_APP_BASE_URL = NA)
  withr::local_options(rNDC.Shiny.max_area_km2 = NULL)
  expect_equal(max_area_km2(), 2000)

  withr::local_envvar(NDC_MAX_AREA_KM2 = "250")
  ndc_setup()
  expect_equal(max_area_km2(), 250)

  options(rNDC.Shiny.max_area_km2 = NULL)
  for (bad in c("abc", "-5", "0", "")) {
    withr::local_envvar(NDC_MAX_AREA_KM2 = bad)
    ndc_setup()
    expect_equal(max_area_km2(), 2000, info = bad)
  }
})

test_that("a selection with too large an area can only be used for Weather: the datasets are refused, with the limits and where to go", {
  mock_check()   # (what the services have is tested in test-availability.R)
  rec <- new.env()
  rec$notes <- list()
  local_mocked_bindings(showNotification = function(ui, type = "default", ...) {
    rec$notes[[length(rec$notes) + 1]] <- list(ui, type)
  })
  big <- transform(square_at(5, 52, 1), source_name = "Big", layer_id = 2L)
  sel <- rbind(square_at(5.5, 52.5, 0.01), big)
  sel$source <- "drawn"

  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    expect_length(selection_issues(), 0)
    selected_polygons(sel)
    session$flushReact()

    # a warning says what is wrong and what is disabled, as soon as the selection is
    expect_named(selection_issues(), "area")
    expect_equal(rec$notes[[1]][[2]], "warning")
    expect_match(rec$notes[[1]][[1]], "The selection is above the limits: The selected polygons cover 7,", fixed = TRUE)
    expect_match(rec$notes[[1]][[1]], "All the datasets except Weather are disabled", fixed = TRUE)
    expect_match(rec$notes[[1]][[1]], "see https://ndc.wur.nl or contact LTER-LIFE@nioo.knaw.nl", fixed = TRUE)
    expect_false(add_enabled())                                     # the Add button is disabled for Land Use
    expect_false(check_dataset_enabled())                           # and so is the check next to it

    # ... and if it is clicked all the same, nothing is added (not even the small polygon)
    session$setInputs(add_dataset = 1)
    expect_equal(nrow(overview()), 0)
    expect_equal(rec$notes[[length(rec$notes)]][[2]], "error")
    expect_match(rec$notes[[length(rec$notes)]][[1]], "Not added: The selected polygons cover", fixed = TRUE)
    expect_match(rec$notes[[length(rec$notes)]][[1]], "or use Weather, which has no area limit", fixed = TRUE)

    # Weather has no area limit: it can be added, for any size
    session$setInputs(selected_dataset = "Weather", weather_mode = "single", weather_date = Sys.Date() - 2)
    session$flushReact()
    expect_true(check_dataset_enabled())
    session$setInputs(check_dataset = 1)                            # (the check next to Add to overview enables Add)
    expect_true(add_enabled())
    session$setInputs(add_dataset = 2)
    expect_equal(overview()$dataset, c("Weather", "Weather"))

    # within the limits again: the datasets can be checked and added
    selected_polygons(sel[1, ])
    session$flushReact()
    expect_length(selection_issues(), 0)
    session$setInputs(selected_dataset = "Land Use")
    session$flushReact()
    expect_true(check_dataset_enabled())
    expect_false(add_enabled())
    session$setInputs(check_dataset = 2)
    expect_true(add_enabled())
    session$setInputs(add_dataset = 3)
    expect_equal(overview()$dataset, c("Weather", "Weather", "Land Use"))
  })
})

test_that("the area is the total of the selected polygons: each of them within the limit is not enough", {
  mock_check()
  halves <- do.call(rbind, lapply(1:3, function(i) transform(square_at(5 + i / 5, 52, 0.5), layer_id = i)))   # about 1,900 km2 each
  halves$source <- "drawn"
  with_server({
    session$setInputs(selected_dataset = "AHN")
    selected_polygons(halves[1, ])
    session$flushReact()
    expect_length(selection_issues(), 0)
    expect_true(check_dataset_enabled())

    selected_polygons(halves)
    session$flushReact()
    expect_named(selection_issues(), "area")
    expect_false(check_dataset_enabled())
  })
})

test_that("too many polygons disable all the datasets, Weather too, and the same warning is not repeated", {
  mock_check()
  rec <- new.env()
  rec$notes <- list()
  local_mocked_bindings(showNotification = function(ui, type = "default", ...) {
    rec$notes[[length(rec$notes) + 1]] <- list(ui, type)
  })
  local_mocked_bindings(max_polygons = function() 2L)
  three <- do.call(rbind, lapply(1:3, function(i) transform(square_at(5 + i / 10, 52, 0.01), layer_id = i)))
  three$source <- "drawn"
  with_server({
    session$setInputs(selected_dataset = "AHN")
    selected_polygons(three)
    session$flushReact()
    expect_equal(selection_issues(), c(count = "3 polygons are selected: at most 2 can be used at once."))
    expect_false(check_dataset_enabled())
    expect_false(add_enabled())
    n <- length(rec$notes)
    expect_match(rec$notes[[n]][[1]], "3 polygons are selected: at most 2 can be used at once.", fixed = TRUE)
    expect_match(rec$notes[[n]][[1]], "All the datasets, Weather included, are disabled", fixed = TRUE)

    # Weather is limited in the number of polygons too
    session$setInputs(selected_dataset = "Weather", weather_mode = "single", weather_date = Sys.Date() - 2)
    session$flushReact()
    expect_false(check_dataset_enabled())
    expect_false(add_enabled())
    session$setInputs(add_dataset = 1)
    expect_equal(nrow(overview()), 0)
    expect_match(rec$notes[[length(rec$notes)]][[1]], "Not added: 3 polygons are selected: at most 2 can be used at once. Select fewer or smaller polygons.", fixed = TRUE)
    expect_false(grepl("or add Weather", rec$notes[[length(rec$notes)]][[1]], fixed = TRUE))   # (it cannot)

    n <- length(rec$notes)
    selected_polygons(three)                                          # the same again: no new warning
    session$flushReact()
    expect_length(rec$notes, n)

    selected_polygons(three[1:2, ])
    session$flushReact()
    expect_length(selection_issues(), 0)
    expect_true(check_dataset_enabled())
    session$setInputs(check_dataset = 1)
    expect_true(add_enabled())
    session$setInputs(add_dataset = 2)
    expect_equal(overview()$dataset, c("Weather", "Weather"))         # (two polygons: within the limit)
  })
})

test_that("the panels to upload and to draw are not disabled, and say what the limits are", {
  html <- as.character(app_ui())
  expect_false(grepl("disabled-category", html, fixed = TRUE))
  expect_match(html, "Upload your own polygon", fixed = TRUE)
  expect_match(html, "Draw your own polygon", fixed = TRUE)
  expect_match(html, "up to 2,000 km2", fixed = TRUE)
  expect_match(html, paste0("at most ", max_upload_features, " polygons"), fixed = TRUE)
  # what the panel of the upload says about the selection (and the one of the drawing, in the same words)
  expect_match(html, "At most 100 polygons can be selected at once, and their total area can be up to 2,000 km2 (Weather has no area limit): while the selected polygons are above these limits, the datasets are disabled (Weather only when there are too many polygons).", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("polygons can be selected at once", html, fixed = TRUE))), 2)
})

test_that("the messages about a limit say where to go", {
  sentence <- function(what) paste0("For ", what, ", see https://ndc.wur.nl or contact LTER-LIFE@nioo.knaw.nl.")
  expect_equal(contact_sentence(), "For help, see https://ndc.wur.nl or contact LTER-LIFE@nioo.knaw.nl.")
  expect_match(retrieve_too_long_message(700), sentence("longer retrievals or help"), fixed = TRUE)
})

test_that("the limits are named as they are set, with links to the website and the address", {
  expect_match(limits_text(), "up to 100 selected polygons, with a total area of up to 2,000 km² (Weather has no area limit)", fixed = TRUE)
  expect_match(limits_text(), "a retrieval of about 30 minutes at the most", fixed = TRUE)
  expect_match(limits_text(), "1000 polygons per file or layer", fixed = TRUE)
  withr::local_options(rNDC.Shiny.max_area_km2 = 750, rNDC.Shiny.max_polygons = 40, rNDC.Shiny.max_request_seconds = 120,
                       shiny.maxRequestSize = 20 * 1024^2)
  expect_match(limits_text(), "up to 40 selected polygons, with a total area of up to 750 km²", fixed = TRUE)
  expect_match(limits_text(), "a retrieval of about 2 minutes", fixed = TRUE)
  expect_match(limits_text(), "uploads of up to 20 MB", fixed = TRUE)

  html <- as.character(contact_note())
  expect_match(html, '<a href="https://ndc.wur.nl" target="_blank" rel="noopener noreferrer">https://ndc.wur.nl</a>', fixed = TRUE)
  expect_match(html, '<a href="mailto:LTER-LIFE@nioo.knaw.nl">LTER-LIFE@nioo.knaw.nl</a>', fixed = TRUE)
  expect_match(html, "up to 40 selected polygons, with a total area of up to 750 km", fixed = TRUE)
})
