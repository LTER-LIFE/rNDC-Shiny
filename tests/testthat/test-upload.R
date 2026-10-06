# Uploading polygons, choosing a layer of a multi-layer GeoPackage, and removing uploads.

upload_files <- function(name, path) data.frame(name = name, size = 1L, type = "", datapath = path)

write_layers <- function(path, layers) {
  for (i in seq_along(layers)) {
    sf::st_write(layers[[i]], path, layer = names(layers)[i], quiet = TRUE)
  }
  path
}

square_geometry <- function(x) fixed_square(1, x, 52.0)[, "geometry"]

# Choose `layer` for the pending GeoPackage `pid` and click its "Import" button
import_layer <- function(session, pid, layer) {
  inputs <- list(layer, 1)
  names(inputs) <- c(paste0("select_gpkg_", pid), paste0("import_gpkg_", pid))
  do.call(session$setInputs, inputs)
}

test_that("an uploaded file with polygons is imported, coloured and listed", {
  path <- write_layers(withr::local_tempfile(fileext = ".gpkg"), list(parcels = square_geometry(5.7)))

  with_server({
    session$setInputs(upload = upload_files("parcels.gpkg", path))
    expect_equal(nrow(uploaded_polys()), 1)
    expect_equal(uploaded_polys()$source_name, "parcels.gpkg_1")
    expect_equal(uploaded_polys()$source, "uploaded")
    expect_match(uploaded_polys()$wkt, "^POLYGON")
    expect_true(!is.na(uploaded_polys()$color))
    expect_length(pending_gpkg(), 0)

    html <- as.character(output$upload_panel$html)
    expect_match(html, "parcels.gpkg_1", fixed = TRUE)
  })
})

test_that("uploaded polygons can be selected by clicking and are removed with their selection", {
  path <- write_layers(withr::local_tempfile(fileext = ".gpkg"), list(parcels = square_geometry(5.7)))

  with_server({
    session$setInputs(upload = upload_files("parcels.gpkg", path))
    lid <- uploaded_polys()$layer_id
    session$setInputs(map_shape_click = map_click(5.705, 52.005, id = lid))
    expect_equal(selected_polygons()$source, "uploaded")

    session$setInputs(remove_uploaded = lid)
    expect_null(uploaded_polys())
    expect_null(selected_polygons())
    expect_match(as.character(output$upload_panel$html), "No uploaded/imported polygon layers yet")
  })
})

test_that("an upload without polygons imports nothing", {
  path <- withr::local_tempfile(fileext = ".geojson")
  sf::st_write(sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326)), path, quiet = TRUE)

  with_server({
    session$setInputs(upload = upload_files("points.geojson", path))
    expect_null(uploaded_polys())
  })
})

test_that("a GeoPackage with several layers waits for a layer to be chosen", {
  path <- write_layers(withr::local_tempfile(fileext = ".gpkg"), list(a = square_geometry(5.7), b = square_geometry(5.9)))

  with_server({
    session$setInputs(upload = upload_files("multi.gpkg", path))
    session$flushReact()  # creates the import observers
    expect_null(uploaded_polys())
    expect_length(pending_gpkg(), 1)
    pid <- names(pending_gpkg())
    expect_equal(pending_gpkg()[[pid]]$layers, c("a", "b"))
    expect_match(as.character(output$upload_panel$html), "GeoPackage layer selection", fixed = TRUE)

    import_layer(session, pid, "b")
    expect_equal(nrow(uploaded_polys()), 1)
    expect_equal(uploaded_polys()$source_name, "multi.gpkg :: b_1")
    expect_equal(pending_gpkg()[[pid]]$imported, "b")
    expect_equal(pending_gpkg()[[pid]]$layers, "a")

    # removing the imported layer offers it again
    session$setInputs(remove_uploaded = uploaded_polys()$layer_id)
    expect_null(uploaded_polys())
    expect_equal(pending_gpkg()[[pid]]$layers, c("a", "b"))
    expect_length(pending_gpkg()[[pid]]$imported, 0)
  })
})

test_that("a layer cannot be imported when the file is gone", {
  path <- write_layers(withr::local_tempfile(fileext = ".gpkg"), list(a = square_geometry(5.7), b = square_geometry(5.9)))

  with_server({
    session$setInputs(upload = upload_files("multi.gpkg", path))
    session$flushReact()
    pid <- names(pending_gpkg())

    unlink(path)
    expect_warning(import_layer(session, pid, "a"), "unable to open database file")  # GDAL, then the app says no
    expect_null(uploaded_polys())
    expect_equal(pending_gpkg()[[pid]]$layers, c("a", "b"))  # still waiting
  })
})
