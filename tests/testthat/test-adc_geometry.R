# The polygon in an AgroDataCube request. (R/adc_geometry.R)

# A polygon with many vertices (a wobbly circle), as a one-row overview row
complex_row <- function(vertices = 600, radius = 0.003) {
  a <- seq(0, 2 * pi, length.out = vertices + 1)
  r <- radius * (1 + 0.05 * sin(7 * a))
  geom <- sf::st_sfc(sf::st_polygon(list(cbind(5.66 + r * cos(a), 51.99 + r * sin(a)))), crs = 4326)
  poly <- sf::st_sf(wkt = sf::st_as_text(geom), source_name = "Own polygon", layer_id = 1L, geometry = geom)
  tibble::tibble(dataset = "Agricultural fields", view = "Geodata", year = 2023L, polygon = "Own polygon",
                 wkt = poly$wkt, polygon_sf = list(poly), date_from = as.Date(NA), date_to = as.Date(NA))
}

test_that("the polygon of a request keeps its WKT when it is short, and is simplified to fit when it is not", {
  short <- overview_row("Agricultural fields", year = 2023L)
  expect_equal(adc_geometry(short), list(wkt = short$wkt, tolerance = 0))

  row <- complex_row(600)
  expect_gt(nchar(row$wkt), adc_max_wkt_chars)
  geometry <- adc_geometry(row)
  expect_lte(nchar(geometry$wkt), adc_max_wkt_chars)
  expect_gt(geometry$tolerance, 0)
  expect_match(adc_simplified_note(geometry), "simplified by up to .* m")
  expect_equal(adc_simplified_note(list(tolerance = 0)), "")

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
    adc_get_all = function(option, params, token, ...) {
      rec$calls[[length(rec$calls) + 1]] <- c(list(option = option), as.list(params))
      geojson_features(1)
    },
    adc_get = function(url, token, ...) {
      rec$calls[[length(rec$calls) + 1]] <- list(option = "AHN", url = url)
      geojson_features(1)
    },
    .package = "rNDC"
  )
  row <- complex_row(600)

  fields <- retrieve_agricultural_fields(row, "t")
  expect_lte(nchar(rec$calls[[1]]$geometry), adc_max_wkt_chars)
  expect_match(fields$ok_msg, "^Retrieved: Agricultural fields \\(polygon simplified by up to")

  # a simple polygon is sent as it is, with the plain message
  plain <- overview_row("Agricultural fields", year = 2023L)
  out <- retrieve_agricultural_fields(plain, "t")
  expect_equal(rec$calls[[2]]$geometry, plain$wkt)
  expect_equal(out$ok_msg, "Retrieved: Agricultural fields")
})
