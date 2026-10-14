# Uploading polygons, choosing a layer of a multi-layer GeoPackage, and removing uploads.

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
    expect_match(html, "<td>parcels.gpkg</td>", fixed = TRUE)  # one entry for the upload, with its features
    expect_match(html, "<td>1</td>", fixed = TRUE)
  })
})

test_that("uploaded polygons can be selected by clicking and are removed with their selection", {
  path <- write_layers(withr::local_tempfile(fileext = ".gpkg"), list(parcels = square_geometry(5.7)))

  with_server({
    session$setInputs(upload = upload_files("parcels.gpkg", path))
    lid <- uploaded_polys()$layer_id
    session$setInputs(map_shape_click = map_click(5.705, 52.005, id = lid))
    expect_equal(selected_polygons()$source, "uploaded")

    session$setInputs(remove_uploaded = uploaded_polys()$upload_id)
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
    session$setInputs(remove_uploaded = uploaded_polys()$upload_id)
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

test_that("a file with several features keeps them apart: one shape each, one entry in the list, removed together", {
  squares <- sf::st_sf(geometry = c(sf::st_geometry(square_geometry(5.7)), sf::st_geometry(square_geometry(5.9)),
                                    sf::st_geometry(square_geometry(6.1))))
  path <- write_layers(withr::local_tempfile(fileext = ".gpkg"), list(parcels = squares))

  with_server({
    session$setInputs(upload = upload_files("parcels.gpkg", path))
    up <- uploaded_polys()
    expect_equal(up$layer_id, 1:3)                      # every feature is its own shape
    expect_equal(up$upload_id, c(1L, 1L, 1L))           # of one upload
    expect_equal(up$source_name, paste0("parcels.gpkg_", 1:3))
    expect_length(unique(up$wkt), 3)

    html <- as.character(output$upload_panel$html)
    expect_equal(lengths(regmatches(html, gregexpr("parcels.gpkg", html, fixed = TRUE))), 1)  # one entry
    expect_match(html, "<td>3</td>", fixed = TRUE)                                              # with its count

    # clicking a shape selects that feature only
    session$setInputs(map_shape_click = map_click(5.905, 52.005, id = 2))
    expect_equal(selected_polygons()$source_name, "parcels.gpkg_2")

    # a second upload gets the next ids, and removing the first takes all its features
    session$setInputs(upload = upload_files("more.gpkg", write_layers(withr::local_tempfile(fileext = ".gpkg"), list(m = square_geometry(6.3)))))
    expect_equal(uploaded_polys()$layer_id, 1:4)
    expect_equal(uploaded_polys()$upload_id, c(1L, 1L, 1L, 2L))

    session$setInputs(remove_uploaded = 1)
    expect_equal(uploaded_polys()$source_name, "more.gpkg_1")
    expect_null(selected_polygons())  # the selected feature went with its upload
  })
})

test_that("too many features in an upload are refused", {
  local_mocked_bindings(max_upload_features = 2L)
  squares <- sf::st_sf(geometry = c(sf::st_geometry(square_geometry(5.7)), sf::st_geometry(square_geometry(5.9)),
                                    sf::st_geometry(square_geometry(6.1))))
  path <- write_layers(withr::local_tempfile(fileext = ".gpkg"), list(parcels = squares))
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))

  with_server({
    session$setInputs(upload = upload_files("parcels.gpkg", path))
    expect_null(uploaded_polys())
  })
  expect_true(any(grepl("at most 2 are accepted", rec$notes)))
})

test_that("too many features in a layer of a GeoPackage are refused too", {
  local_mocked_bindings(max_upload_features = 2L)
  squares <- sf::st_sf(geometry = c(sf::st_geometry(square_geometry(5.7)), sf::st_geometry(square_geometry(5.9)),
                                    sf::st_geometry(square_geometry(6.1))))
  path <- write_layers(withr::local_tempfile(fileext = ".gpkg"), list(big = squares, small = square_geometry(5.7)))
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))

  with_server({
    session$setInputs(upload = upload_files("multi.gpkg", path))
    session$flushReact()
    pid <- names(pending_gpkg())
    import_layer(session, pid, "big")
    expect_null(uploaded_polys())
    expect_equal(pending_gpkg()[[pid]]$layers, c("big", "small"))  # still on offer

    import_layer(session, pid, "small")
    expect_equal(nrow(uploaded_polys()), 1)
  })
  expect_true(any(grepl("Layer big has 3 polygons", rec$notes)))
})
