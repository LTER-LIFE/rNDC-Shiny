# Project layers in the app: LTER classes and SNL parcels streamed by map view.

lter_polygons <- function() {
  geom <- c(sf::st_geometry(fixed_square(1, 5.70, 52.00)), sf::st_geometry(fixed_square(2, 5.80, 52.10)),
            sf::st_geometry(fixed_square(3, 5.90, 52.20)))
  sf::st_sf(name = c("Loobos", "Nestkast 1", "Nestkast 2"), ndc_id = c("1", "2", "3"),
            project_class = c("Loobos", "Nestboxes", "Nestboxes"), geometry = geom)
}

test_that("choosing an LTER project shows its polygons, named and numbered", {
  local_mocked_bindings(get_lter_data = function() list(data = lter_polygons(), error = NULL))

  with_server({
    session$setInputs(ndc_project = "lter:Nestboxes")
    expect_equal(active_project(), "lter:Nestboxes")
    expect_equal(fixed_polys()$source_name, c("Nestboxes_1", "Nestboxes_2"))
    expect_equal(fixed_polys()$layer_id, 1:2)
    expect_equal(fixed_polys()$ndc_id, c("2", "3"))
    expect_true(all(grepl("^POLYGON", fixed_polys()$wkt)))

    # select one: the NDVI statistics can find the right collection from its name
    session$setInputs(map_shape_click = map_click(5.805, 52.105, id = 1))
    expect_equal(selected_polygons()$source_name, "Nestboxes_1")
    expect_equal(detect_ndvi_collection(selected_polygons()$source_name), "ndvi-lter")
  })
})

test_that("switching project starts from a clean map; choosing none clears everything", {
  local_mocked_bindings(get_lter_data = function() list(data = lter_polygons(), error = NULL))

  with_server({
    session$setInputs(ndc_project = "lter:Nestboxes")
    session$setInputs(map_shape_click = map_click(5.805, 52.105, id = 1))
    expect_equal(nrow(selected_polygons()), 1)

    session$setInputs(ndc_project = "lter:Loobos")
    expect_equal(fixed_polys()$source_name, "Loobos_1")
    expect_null(selected_polygons())

    session$setInputs(ndc_project = "")
    expect_null(fixed_polys())
    expect_null(active_project())
  })
})

test_that("a project that cannot be loaded leaves the map empty", {
  local_mocked_bindings(get_lter_data = function() list(data = NULL, error = "boom"))
  with_server({
    session$setInputs(ndc_project = "lter:Nestboxes")
    expect_null(fixed_polys())
  })

  local_mocked_bindings(get_lter_data = function() list(data = lter_polygons(), error = NULL))
  with_server({
    session$setInputs(ndc_project = "lter:Nutnet")  # no such class in the data
    expect_null(fixed_polys())
  })
})

view <- function(zoom, west = 5.70, south = 52.00, east = 5.72, north = 52.02) {
  list(zoom = zoom, bounds = list(west = west, south = south, east = east, north = north))
}

snl_parcels <- function(n = 3) {
  geom <- do.call(c, lapply(seq_len(n), function(i) sf::st_geometry(fixed_square(i, 5.70 + i * 0.001, 52.0, size = 0.0005))))
  sf::st_sf(ndc_id = as.character(seq_len(n)), geometry = geom)
}

test_that("SNL parcels are fetched for the map view once zoomed in, after the view has settled", {
  rec <- new.env()
  rec$requested <- list()
  local_mocked_bindings(fetch_snl_bbox = function(bbox) {
    rec$requested[[length(rec$requested) + 1]] <- bbox
    list(status = "ok", data = snl_parcels(3), error = NULL)
  })

  with_server({
    session$setInputs(ndc_project = "snl")
    expect_equal(active_project(), "snl")
    expect_length(rec$requested, 0)  # no view yet

    v <- view(zoom = 14)
    session$setInputs(map_zoom = v$zoom, map_bounds = v$bounds)
    session$elapse(100)
    expect_length(rec$requested, 0)  # still debouncing
    session$elapse(600)
    expect_length(rec$requested, 1)
    expect_equal(rec$requested[[1]], c(5.70, 52.00, 5.72, 52.02))

    expect_equal(fixed_polys()$source_name, paste0("SNL parcel_", 1:3))
    expect_equal(snl_status_msg()$type, "ok")
    expect_match(snl_status_msg()$text, "Showing 3 SNL parcels in view")
    expect_match(as.character(output$snl_status$html), "Showing 3 SNL parcels")

    # the same view again does not fetch again
    session$setInputs(map_bounds = c(v$bounds, nonce = 1))
    session$elapse(700)
    expect_length(rec$requested, 1)

    # a different view does
    session$setInputs(map_bounds = list(west = 5.71, south = 52.01, east = 5.73, north = 52.03))
    session$elapse(700)
    expect_length(rec$requested, 2)
  })
})

test_that("zooming out clears the SNL parcels and asks to zoom in", {
  local_mocked_bindings(fetch_snl_bbox = function(bbox) list(status = "ok", data = snl_parcels(2), error = NULL))

  with_server({
    session$setInputs(ndc_project = "snl")
    v <- view(zoom = 14)
    session$setInputs(map_zoom = v$zoom, map_bounds = v$bounds)
    session$elapse(700)
    expect_equal(nrow(fixed_polys()), 2)

    session$setInputs(map_zoom = snl_min_zoom - 3)
    session$elapse(700)
    expect_null(fixed_polys())
    expect_equal(snl_status_msg()$type, "hint")
    expect_match(snl_status_msg()$text, "Zoom in")
  })
})

test_that("an SNL view without parcels, or with a failing request, is reported", {
  local_mocked_bindings(fetch_snl_bbox = function(bbox) list(status = "empty", data = NULL, error = NULL))
  with_server({
    session$setInputs(ndc_project = "snl")
    v <- view(zoom = 14)
    session$setInputs(map_zoom = v$zoom, map_bounds = v$bounds)
    session$elapse(700)
    expect_equal(snl_status_msg()$type, "empty")
    expect_null(fixed_polys())
  })

  local_mocked_bindings(fetch_snl_bbox = function(bbox) list(status = "error", data = NULL, error = "HTTP 500"))
  with_server({
    session$setInputs(ndc_project = "snl")
    v <- view(zoom = 14)
    session$setInputs(map_zoom = v$zoom, map_bounds = v$bounds)
    session$elapse(700)
    expect_equal(snl_status_msg()$type, "error")
    expect_match(snl_status_msg()$text, "Could not load SNL parcels: HTTP 500", fixed = TRUE)
  })
})

test_that("SNL parcels stop streaming when another project is chosen", {
  rec <- new.env()
  rec$n <- 0
  local_mocked_bindings(get_lter_data = function() list(data = lter_polygons(), error = NULL),
                        fetch_snl_bbox = function(bbox) { rec$n <- rec$n + 1; list(status = "ok", data = snl_parcels(1), error = NULL) })
  with_server({
    session$setInputs(ndc_project = "snl")
    session$setInputs(ndc_project = "lter:Loobos")
    v <- view(zoom = 14)
    session$setInputs(map_zoom = v$zoom, map_bounds = v$bounds)
    session$elapse(700)
    expect_equal(rec$n, 0)
    expect_equal(fixed_polys()$source_name, "Loobos_1")
    expect_null(output$snl_status$html)
  })
})
