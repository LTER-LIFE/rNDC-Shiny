# One row of the overview: the data, the files with their names, and the reference polygon. (R/retrieve_row.R)

test_that("retrieve_row returns the data without writing anything when there is no work folder", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("fields")) |>
    webmockr::to_return(body = json_body(geojson_features(1:3, "fieldid")), headers = json_header)

  res <- retrieve_row(overview_row("Agricultural fields", year = 2023L), 7, workdir = NULL, ndc_token = "t", adc_token = "t")
  expect_equal(res$name, "Agricultural fields_7")
  expect_equal(nrow(res$data), 3)
  expect_equal(res$messages, "Retrieved: Agricultural fields")
  expect_length(res$manifest, 1)
  expect_equal(res$manifest[[1]]$status, "ok")
  expect_true(is.na(res$manifest[[1]]$file_path))
})

test_that("retrieve_row writes the data, and the reference polygon once per polygon", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("fields")) |>
    webmockr::to_return(body = json_body(geojson_features(1:3, "fieldid")), headers = json_header)
  local_mocked_bindings(ndc_ahn_stats = function(roi, ..., save_params = NULL) {
    if (!is.null(save_params)) writeLines("{}", save_params)
    tibble::tibble(layer = "dtm", year = "2019", area = 1, min = 0, max = 1, mean = 0.5, sd = 0.1, n_cells = 4)
  }, .package = "rNDC")
  dir <- withr::local_tempdir()

  first <- retrieve_row(overview_row("Agricultural fields", year = 2023L, name = "Area A"), 1, dir, "t", "t")
  second <- retrieve_row(overview_row("AHN", "Statistics", name = "Area A"), 2, dir, "t", "t")

  expect_equal(vapply(first$manifest, function(m) m$dataset, ""), c("Area", "Agricultural fields"))
  expect_equal(vapply(second$manifest, function(m) m$dataset, ""), "AHN")  # polygon already exported
  expect_setequal(list.files(dir), c("area_a.gpkg", "agricultural_fields_geodata_area_a.gpkg", "ahn_statistics_area_a.csv", "parameters"))
  expect_setequal(list.files(file.path(dir, "parameters")),
                  c("agricultural_fields_geodata_area_a.json", "ahn_statistics_area_a.json"))   # next to the data, by the same name
  expect_equal(first$manifest[[2]]$parameters, "parameters/agricultural_fields_geodata_area_a.json")
  expect_equal(first$manifest[[1]]$note, "selected polygon geometry")
  expect_equal(first$manifest[[2]]$file_path, "agricultural_fields_geodata_area_a.gpkg")
  expect_equal(utils::read.csv(file.path(dir, "ahn_statistics_area_a.csv"))$polygon, "Area A")
})

test_that("retrieve_row reports an error of the retrieval or of the writing for that row only", {
  local_mocked_bindings(ndc_landuse_raster = function(...) stop("boom", call. = FALSE), .package = "rNDC")
  res <- retrieve_row(overview_row("Land Use", year = 2024L), 3, workdir = NULL, ndc_token = "t", adc_token = "t")
  expect_null(res$data)
  expect_equal(res$messages, "Failed: Land Use - boom")
  expect_equal(res$manifest[[1]]$status, "failed")
  expect_equal(res$manifest[[1]]$note, "boom")

  # A writer that cannot write: the data is still returned, with the message for it
  local_mocked_bindings(
    ndc_landuse_raster = function(...) list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4)),
    .package = "rNDC"
  )
  local_mocked_bindings(export_write_data = function(...) FALSE)
  res <- retrieve_row(overview_row("Land Use", year = 2024L), 3, withr::local_tempdir(), "t", "t")
  expect_s4_class(res$data, "SpatRaster")
  expect_equal(res$messages, "Failed: Land Use - could not write output")
  expect_equal(res$manifest[[2]]$status, "failed")
})

test_that("a writer that throws is reported as a failure of that dataset, keeping the data", {
  local_mocked_bindings(ndc_landuse_raster = function(...) list(stack = terra::rast(nrows = 2, ncols = 2, vals = 1:4)),
                        .package = "rNDC")
  local_mocked_bindings(export_write_data = function(...) stop("disk full", call. = FALSE))
  res <- retrieve_row(overview_row("Land Use", year = 2024L), 1, withr::local_tempdir(), "t", "t")
  expect_s4_class(res$data, "SpatRaster")  # as before the refactoring: the data survive a failing write
  expect_equal(res$messages, "Failed: Land Use - disk full")
})

test_that("export_unique_file keeps the plain name and otherwise appends the period, or a number", {
  dir <- withr::local_tempdir()
  plain <- export_unique_file(dir, "ahn_statistics_p", "csv", "2024")
  expect_equal(basename(plain), "ahn_statistics_p.csv")

  file.create(plain)
  dated <- export_unique_file(dir, "ahn_statistics_p", "csv", "2024")
  expect_equal(basename(dated), "ahn_statistics_p_2024.csv")
  expect_equal(basename(export_unique_file(dir, "ahn_statistics_p", "csv", "2024-05-01 - 2024-05-31")),
               "ahn_statistics_p_2024-05-01_to_2024-05-31.csv")

  file.create(dated)
  expect_equal(basename(export_unique_file(dir, "ahn_statistics_p", "csv", "2024")), "ahn_statistics_p_2.csv")
  expect_equal(basename(export_unique_file(dir, "ahn_statistics_p", "csv", "")), "ahn_statistics_p_2.csv")

  # Another extension or base name is not a collision
  expect_equal(basename(export_unique_file(dir, "ahn_statistics_p", "gpkg", "2024")), "ahn_statistics_p.gpkg")
})

test_that("rows that differ only in year do not overwrite each other's file", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("fields")) |>
    webmockr::to_return(body = json_body(geojson_features(1:2)), headers = json_header) |>
    webmockr::to_return(body = json_body(geojson_features(1:5)), headers = json_header)
  dir <- withr::local_tempdir()

  y2024 <- retrieve_row(overview_row("Agricultural fields", year = 2024L), 1, dir, "t", "t")
  y2023 <- retrieve_row(overview_row("Agricultural fields", year = 2023L), 2, dir, "t", "t")

  files <- list.files(dir, pattern = "^agricultural")
  expect_setequal(files, c("agricultural_fields_geodata_own_polygon.gpkg",
                           "agricultural_fields_geodata_own_polygon_2023.gpkg"))
  # The summary says which file belongs to which year, and each file holds its own data
  expect_equal(y2024$manifest[[2]]$file_path, "agricultural_fields_geodata_own_polygon.gpkg")
  expect_equal(y2023$manifest[[1]]$file_path, "agricultural_fields_geodata_own_polygon_2023.gpkg")
  expect_equal(nrow(sf::st_read(file.path(dir, y2024$manifest[[2]]$file_path), quiet = TRUE)), 2)
  expect_equal(nrow(sf::st_read(file.path(dir, y2023$manifest[[1]]$file_path), quiet = TRUE)), 5)
})

other_polygon <- function() {
  sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = 6.1, ymin = 52.1, xmax = 6.2, ymax = 52.2), crs = sf::st_crs(4326))))
}

test_that("the reference polygon is exported once for the same polygon, and again for another with the same name", {
  dir <- withr::local_tempdir()
  same <- overview_row("AHN", name = "Area A")
  other <- same
  other$polygon_sf <- list(other_polygon())

  first <- export_reference_polygon(same, dir, "area_a", "2024")
  again <- export_reference_polygon(same, dir, "area_a", "2023")  # the same polygon: nothing new
  second <- export_reference_polygon(other, dir, "area_a", "2024")  # another polygon with the same name
  third <- export_reference_polygon(other, dir, "area_a", "2025")

  expect_equal(first$file_path, "area_a.gpkg")
  expect_null(again)
  expect_equal(second$file_path, "area_a_2.gpkg")
  expect_null(third)
  expect_setequal(list.files(dir), c("area_a.gpkg", "area_a_2.gpkg"))
  expect_true(all(c(first$status, second$status) == "ok"))
  # each file holds its own polygon
  expect_equal(sf::st_bbox(sf::st_read(file.path(dir, "area_a_2.gpkg"), quiet = TRUE))[["xmin"]], 6.1)
  expect_equal(sf::st_bbox(sf::st_read(file.path(dir, "area_a.gpkg"), quiet = TRUE))[["xmin"]], 5.75)
})

test_that("a polygon file with the name of another is not mistaken for it", {
  dir <- withr::local_tempdir()
  geom <- sf::st_sf(polygon = "a", geometry = sf::st_geometry(selected_polygon()))
  expect_equal(basename(export_polygon_file(dir, "p", geom)), "p.gpkg")  # nothing there yet

  sf::st_write(geom, file.path(dir, "p.gpkg"), quiet = TRUE)
  expect_null(export_polygon_file(dir, "p", geom))
  expect_true(export_same_polygon(file.path(dir, "p.gpkg"), geom))
  # the same polygon in another CRS is still the same polygon
  expect_true(export_same_polygon(file.path(dir, "p.gpkg"), sf::st_transform(geom, 28992)))

  other <- sf::st_sf(polygon = "b", geometry = sf::st_geometry(other_polygon()))
  expect_false(export_same_polygon(file.path(dir, "p.gpkg"), other))
  expect_equal(basename(export_polygon_file(dir, "p", other)), "p_2.gpkg")

  writeLines("not a geopackage", file.path(dir, "broken.gpkg"))
  expect_false(export_same_polygon(file.path(dir, "broken.gpkg"), geom))
})
