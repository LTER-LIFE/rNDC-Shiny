# Limits on drawn and uploaded polygons (R/limits.R), and the polygon in an AgroDataCube request.

square_at <- function(x, y, size) {
  geom <- sf::st_as_sfc(sf::st_bbox(c(xmin = x, ymin = y, xmax = x + size, ymax = y + size), crs = sf::st_crs(4326)))
  sf::st_sf(wkt = sf::st_as_text(geom), source_name = "Own polygon", layer_id = 1L, geometry = geom)
}

# A polygon with many vertices (a wobbly circle), as a one-row overview row
complex_row <- function(vertices = 600, radius = 0.003) {
  a <- seq(0, 2 * pi, length.out = vertices + 1)
  r <- radius * (1 + 0.05 * sin(7 * a))
  geom <- sf::st_sfc(sf::st_polygon(list(cbind(5.66 + r * cos(a), 51.99 + r * sin(a)))), crs = 4326)
  poly <- sf::st_sf(wkt = sf::st_as_text(geom), source_name = "Own polygon", layer_id = 1L, geometry = geom)
  tibble::tibble(dataset = "Agricultural fields", view = "Geodata", year = 2023L, polygon = "Own polygon",
                 wkt = poly$wkt, polygon_sf = list(poly), date_from = as.Date(NA), date_to = as.Date(NA))
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
  expect_equal(polygons_too_large(sel, limit = 1000), "Big")
  expect_equal(polygons_too_large(sel, limit = 0.1), c("Own polygon", "Big"))
  expect_equal(polygons_too_large(sel, limit = 1e6), character(0))
  expect_equal(polygons_too_large(NULL), character(0))
  expect_match(too_large_message("Big", 1000), "the polygon Big is larger than the limit of 1,000 km")
  expect_match(too_large_message(c("A", "B"), 5), "the polygons A, B are larger")
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

test_that("the polygon of a request keeps its WKT when it is short, and is simplified to fit when it is not", {
  short <- overview_row("Agricultural fields", year = 2023L)
  expect_equal(adc_geometry(short), list(wkt = short$wkt, tolerance = 0))

  row <- complex_row(600)
  expect_gt(nchar(row$wkt), adc_max_wkt_chars)
  geometry <- adc_geometry(row)
  expect_lte(nchar(geometry$wkt), adc_max_wkt_chars)
  expect_gt(geometry$tolerance, 0)
  expect_match(simplified_note(geometry), "simplified by up to .* m")
  expect_equal(simplified_note(list(tolerance = 0)), "")

  # the simplified polygon is still the same place: its area is within a few percent
  original <- polygon_area_km2(row$polygon_sf[[1]])
  simple <- polygon_area_km2(sf::st_as_sfc(geometry$wkt, crs = 4326))
  expect_equal(simple, original, tolerance = 0.05)

  # a little too long only needs fewer decimals
  decimals <- adc_geometry(row, max_chars = nchar(sf::st_as_text(sf::st_geometry(row$polygon_sf[[1]]), digits = 6)) + 1)
  expect_equal(decimals$tolerance, 0)
  expect_lt(nchar(decimals$wkt), nchar(row$wkt))

  expect_error(adc_geometry(row, max_chars = 40), "too complex for AgroDataCube")
})

test_that("the AgroDataCube datasets send the shortened polygon and say so", {
  rec <- new.env()
  rec$calls <- list()
  local_mocked_bindings(
    adc_get_all = function(option, params, token) {
      rec$calls[[length(rec$calls) + 1]] <- c(list(option = option), as.list(params))
      geojson_features(1)
    },
    adc_get = function(url, token) {
      rec$calls[[length(rec$calls) + 1]] <- list(option = "AHN", url = url)
      geojson_features(1)
    },
    .package = "rNDC"
  )
  row <- complex_row(600)

  fields <- retrieve_agricultural_fields(row, "t")
  expect_lte(nchar(rec$calls[[1]]$geometry), adc_max_wkt_chars)
  expect_match(fields$ok_msg, "^Retrieved: Agricultural fields \\(polygon simplified by up to")

  row$dataset <- "Soil map"
  soil <- retrieve_soil_map(row, "t")
  expect_lte(nchar(rec$calls[[2]]$geometry), adc_max_wkt_chars)
  expect_match(soil$ok_msg, "simplified")

  row$dataset <- "AHN"
  ahn <- retrieve_ahn(row, "t")
  expect_lte(nchar(rec$calls[[3]]$url), 8000)
  expect_match(ahn$ok_msg, "simplified")

  # a simple polygon is sent as it is, with the plain message
  plain <- overview_row("Agricultural fields", year = 2023L)
  out <- retrieve_agricultural_fields(plain, "t")
  expect_equal(rec$calls[[4]]$geometry, plain$wkt)
  expect_equal(out$ok_msg, "Retrieved: Agricultural fields")
})

test_that("the panels to upload and to draw are not disabled, and say what the limits are", {
  html <- as.character(app_ui())
  expect_false(grepl("disabled-category", html, fixed = TRUE))
  expect_match(html, "Upload your own polygon", fixed = TRUE)
  expect_match(html, "Draw your own polygon", fixed = TRUE)
  expect_match(html, "up to 1,000 km2", fixed = TRUE)
  expect_match(html, paste0("at most ", max_upload_features, " polygons"), fixed = TRUE)
})
