# Live checks against the real APIs. They are skipped by default (and on CRAN) and run with
#   RNDC_LIVE_TESTS=true NDC_TOKEN=... ADC_TOKEN=... Rscript -e 'devtools::test(filter = "live")'
# They detect changes in the APIs that the offline tests cannot see, such as renamed collections.

skip_unless_live <- function() {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("RNDC_LIVE_TESTS"), "true"), "live API tests are opt-in (RNDC_LIVE_TESTS=true)")
  skip_if(!nzchar(Sys.getenv("NDC_TOKEN")) || !nzchar(Sys.getenv("ADC_TOKEN")),
          "NDC_TOKEN and ADC_TOKEN are required")
}

test_that("the collections used by the app exist", {
  skip_unless_live()
  expect_true(all(c(ndc_lter_collection, ndc_snl_collection, "ndvi-lter", "ndvi-snl", "lgn", nitrogen_layer_choices)
                  %in% rNDC::ndc_datasets()))
})

test_that("the LTER projects are found and classified", {
  skip_unless_live()
  local_clean_cache()
  res <- get_lter_data()
  expect_null(res$error)
  expect_setequal(stats::na.omit(unique(res$data$project_class)), lter_class_levels)
  expect_true("ndc_id" %in% names(res$data))
})

test_that("the LTER projects cover every study site of rNDC", {
  # The app has no menu of its own for rNDC::ndc_sites(): the LTER projects are the same polygons (with an ndc_id,
  # so with NDVI statistics). If a site is added to rNDC and not to the lter collection, this fails.
  skip_unless_live()
  local_clean_cache()
  lter <- get_lter_data()$data
  sites <- lapply(rNDC::ndc_sites(), function(layer) rNDC::ndc_sites(layer))
  expect_equal(nrow(lter), sum(vapply(sites, nrow, 1L)))
  for (site in sites) {
    site <- sf::st_transform(sf::st_geometry(site), sf::st_crs(lter))
    expect_true(all(lengths(sf::st_intersects(site, lter)) >= 1))
  }
})

test_that("SNL parcels can be fetched for a map view", {
  skip_unless_live()
  res <- fetch_snl_bbox(c(5.75, 52.05, 5.76, 52.06))
  expect_equal(res$status, "ok")
  expect_true(all(c("ndc_id", "name") %in% names(res$data)))
})

test_that("NDVI statistics belong to the selected LTER feature only", {
  skip_unless_live()
  local_clean_cache()
  lter <- get_lter_data()$data
  poly <- add_wkt_column(lter[lter$project_class == "Loobos", ][1, ])
  res <- fetch_ndvi_stats_monthly(poly, "ndvi-lter", as.Date("2024-05-01"), as.Date("2024-07-31"))
  expect_equal(res$status, "ok")
  expect_equal(anyDuplicated(res$data$month), 0)
  expect_true(all(res$data$ndvi_mean > -1 & res$data$ndvi_mean < 1))
})

test_that("the nitrogen years are available", {
  skip_unless_live()
  local_clean_cache()
  expect_true(all(c("2024", "2025") %in% get_nitrogen_years()))
})

test_that("AgroDataCube results are fetched completely", {
  skip_unless_live()
  # About 700 fields in this area: more than the default page of the API
  poly <- "POLYGON((5.70 52.00,5.85 52.00,5.85 52.10,5.70 52.10,5.70 52.00))"
  res <- rNDC::adc_get_all("Fields", c(geometry = poly, epsg = "4326", year = "2024", output_epsg = "4326"),
                     token = Sys.getenv("ADC_TOKEN"))
  expect_gt(length(res$features), 100)
})

test_that("AgroDataCube accepts a polygon with many vertices once it is shortened", {
  # The server refuses URLs above about 8000 characters (500 vertices fail, 300 work): adc_max_wkt_chars is below that.
  skip_unless_live()
  a <- seq(0, 2 * pi, length.out = 801)
  r <- 0.003 * (1 + 0.05 * sin(7 * a))
  geom <- sf::st_sfc(sf::st_polygon(list(cbind(5.66 + r * cos(a), 51.99 + r * sin(a)))), crs = 4326)
  poly <- sf::st_sf(geometry = geom)
  row <- tibble::tibble(dataset = "Agricultural fields", view = "Geodata", year = 2023L, polygon = "Own polygon",
                        wkt = sf::st_as_text(geom), polygon_sf = list(poly),
                        date_from = as.Date(NA), date_to = as.Date(NA))
  expect_gt(nchar(row$wkt), 10000)

  outcome <- retrieve_agricultural_fields(row, Sys.getenv("ADC_TOKEN"))
  expect_gt(nrow(outcome$data), 0)
  expect_match(outcome$ok_msg, "simplified")
})

test_that("the availability check finds data in the Netherlands and none abroad", {
  skip_unless_live()
  local_clean_cache()
  square <- function(x, y, name) {
    geom <- sf::st_as_sfc(sf::st_bbox(c(xmin = x, ymin = y, xmax = x + 0.01, ymax = y + 0.01), crs = sf::st_crs(4326)))
    sf::st_sf(source_name = name, geometry = geom)
  }
  years <- list("Agricultural fields" = 2023, "Land Use" = "2024", "Nitrogen" = "2024")
  nl <- check_area_availability(square(5.66, 51.99, "Own polygon_1"), Sys.getenv("NDC_TOKEN"), Sys.getenv("ADC_TOKEN"), years)
  yes <- c("Weather", "Soil map", "AHN", "Agricultural fields", "Land Use", "Nitrogen", "NDVI")
  expect_equal(nl$status[match(yes, nl$dataset)], rep("yes", length(yes)), info = paste(nl$detail, collapse = " | "))

  abroad <- check_area_availability(square(4.6, 50.6, "Own polygon_1"), Sys.getenv("NDC_TOKEN"), Sys.getenv("ADC_TOKEN"), years)
  no <- c("Soil map", "AHN", "Agricultural fields", "Land Use", "Nitrogen", "NDVI")
  expect_equal(abroad$status[match(no, abroad$dataset)], rep("no", length(no)), info = paste(abroad$detail, collapse = " | "))
})

test_that("there is data at the earliest dates and years that the app offers", {
  skip_unless_live()
  # NDVI: GroenMonitor starts on 2017-05-26 (ndvi_min_month); it is not downloaded here, as that takes a minute.
  poly <- "POLYGON((5.70 52.00,5.72 52.00,5.72 52.02,5.70 52.02,5.70 52.00))"
  fields <- rNDC::adc_get_all("Fields", c(geometry = poly, epsg = "4326", year = as.character(fields_min_year),
                                    output_epsg = "4326"), token = Sys.getenv("ADC_TOKEN"))
  expect_gt(length(fields$features), 0)

  station <- rNDC::get_closest_meteostation(poly, token = Sys.getenv("ADC_TOKEN"))$closest_id
  meteo <- suppressWarnings(rNDC::get_meteo_for_date(station, weather_min_date, Sys.getenv("ADC_TOKEN")))
  expect_false(is.null(meteo))
})

test_that("the land use years come from the NatureDataCube", {
  skip_unless_live()
  local_clean_cache()
  expect_true(as.character(landuse_default_year) %in% get_landuse_years())
})

test_that("the availability check finds raster items for a place in the Netherlands, and none elsewhere", {
  skip_unless_live()
  local_clean_cache()
  here <- sf::st_geometry(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
    c(xmin = 5.74, ymin = 52.05, xmax = 5.76, ymax = 52.07), crs = sf::st_crs(4326)))))
  elsewhere <- sf::st_geometry(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
    c(xmin = -0.2, ymin = 51.4, xmax = -0.1, ymax = 51.5), crs = sf::st_crs(4326)))))  # London

  expect_gt(count_items(ndc_landuse_collection, here), 0)
  expect_equal(count_items(ndc_landuse_collection, elsewhere), 0)

  # one search for all raster collections
  table <- rNDC::stac_availability(c(ndc_landuse_collection, nitrogen_layer_choices), here, Sys.getenv("NDC_TOKEN"))
  expect_true(all(c(ndc_landuse_collection, nitrogen_layer_choices) %in% table$collection))
  expect_true("2024" %in% table$year)
  expect_equal(nrow(rNDC::stac_availability(c(ndc_landuse_collection, nitrogen_layer_choices), elsewhere, Sys.getenv("NDC_TOKEN"))), 0)
})
