# Selecting polygons on the map: clicking, "Deselect all", and drawing.

two_polygons <- function() rbind(fixed_square(1, 5.70, 52.00), fixed_square(2, 5.80, 52.10))

test_that("clicking a project polygon selects it, and clicking it again deselects it", {
  with_server({
    fixed_polys(two_polygons())

    session$setInputs(map_shape_click = map_click(5.705, 52.005, id = 1))
    expect_equal(selected_polygons()$source_name, "Project_1")
    expect_equal(selected_polygons()$source, "fixed")

    session$setInputs(map_shape_click = map_click(5.805, 52.105, id = 2))
    expect_equal(selected_polygons()$source_name, c("Project_1", "Project_2"))

    session$setInputs(map_shape_click = map_click(5.705, 52.005, id = 1))
    expect_equal(selected_polygons()$source_name, "Project_2")

    session$setInputs(map_shape_click = map_click(5.805, 52.105, id = 2))
    expect_null(selected_polygons())
  })
})

test_that("a click without a polygon id selects the polygon under the pointer", {
  with_server({
    fixed_polys(two_polygons())
    session$setInputs(map_shape_click = map_click(5.805, 52.105))
    expect_equal(selected_polygons()$source_name, "Project_2")
  })
})

test_that("a click outside every polygon changes nothing", {
  with_server({
    fixed_polys(two_polygons())
    session$setInputs(map_shape_click = map_click(4.0, 51.0))
    expect_null(selected_polygons())
  })
})

test_that("'Deselect all' clears the selection but keeps the project layer", {
  with_server({
    fixed_polys(two_polygons())
    session$setInputs(map_shape_click = map_click(5.705, 52.005, id = 1))
    expect_equal(nrow(selected_polygons()), 1)

    session$setInputs(ndc_deselect_all = 1)
    expect_null(selected_polygons())
    expect_equal(nrow(fixed_polys()), 2)
  })
})

drawn_feature <- function(x = 5, y = 52, size = 0.1) {
  list(type = "Feature",
       geometry = list(type = "Polygon",
                       coordinates = list(list(list(x, y), list(x + size, y), list(x + size, y + size),
                                               list(x, y + size), list(x, y)))))
}

test_that("a drawn polygon is named, kept and selected", {
  with_server({
    session$setInputs(map_draw_new_feature = drawn_feature())
    expect_equal(nrow(drawn_features()), 1)
    expect_equal(drawn_features()$source_name, "Own polygon_1")
    expect_equal(selected_polygons()$source, "drawn")
    expect_match(drawn_features()$wkt, "^POLYGON")

    session$setInputs(map_draw_new_feature = drawn_feature(6, 52))
    expect_equal(drawn_features()$source_name, c("Own polygon_1", "Own polygon_2"))
    expect_equal(drawn_features()$layer_id, c(1L, 2L))
    expect_equal(selected_polygons()$source_name, "Own polygon_2")  # a new drawing starts a new selection
  })
})

test_that("drawing replaces a selected project polygon", {
  with_server({
    fixed_polys(two_polygons())
    session$setInputs(map_shape_click = map_click(5.705, 52.005, id = 1))
    session$setInputs(map_draw_new_feature = drawn_feature())
    expect_equal(selected_polygons()$source, "drawn")
    expect_null(fixed_polys())
    expect_null(active_project())
  })
})

test_that("deleting drawn polygons removes them and their selection", {
  with_server({
    session$setInputs(map_draw_new_feature = drawn_feature(5, 52))
    session$setInputs(map_draw_new_feature = drawn_feature(6, 52))
    expect_equal(nrow(drawn_features()), 2)

    session$setInputs(map_draw_deleted_features = list(features = list(drawn_feature(5, 52))))
    expect_equal(drawn_features()$source_name, "Own polygon_2")
    expect_equal(selected_polygons()$source_name, "Own polygon_2")

    session$setInputs(map_draw_deleted_features = list(features = list(drawn_feature(6, 52)), nonce = 1))
    expect_null(drawn_features())
    expect_null(selected_polygons())
  })
})

test_that("deleting without details removes all drawn polygons, and with none drawn clears the selection", {
  with_server({
    session$setInputs(map_draw_new_feature = drawn_feature())
    session$setInputs(map_draw_deleted_features = list(features = list(), nonce = 1))
    expect_null(drawn_features())
    expect_null(selected_polygons())

    selected_polygons(selected_polygon())
    session$setInputs(map_draw_deleted_features = list(features = list(), nonce = 2))
    expect_null(selected_polygons())
  })
})
