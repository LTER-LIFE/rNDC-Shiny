# The overview table ("shopping cart") and the download flow.

three_rows <- function() {
  dplyr::bind_rows(overview_row("AHN", "Statistics"),
                   overview_row("Soil map"),
                   overview_row("Land Use", year = 2024L, name = "Other polygon"))
}

test_that("the overview table lists the datasets, views, dates and polygons", {
  with_server({
    expect_match(as.character(output$overview_table$html), "No datasets added yet")

    overview(dplyr::bind_rows(three_rows(),
                              overview_row("Weather", "Statistics", from = as.Date("2024-05-01"), to = as.Date("2024-05-31"))))
    session$flushReact()
    html <- as.character(output$overview_table$html)
    expect_match(html, "AHN", fixed = TRUE)
    expect_match(html, "Other polygon", fixed = TRUE)
    expect_match(html, "<td>2024</td>", fixed = TRUE)                       # a year
    expect_match(html, "2024-05-01 - 2024-05-31", fixed = TRUE)              # a period
    expect_equal(lengths(regmatches(html, gregexpr("btn-delete", html))), 4)  # one delete button per row
  })
})

test_that("a row can be deleted; a row that does not exist is refused", {
  with_server({
    overview(three_rows())

    session$setInputs(delete_row = 2)
    expect_equal(overview()$dataset, c("AHN", "Land Use"))

    session$setInputs(delete_row = 9)  # out of range
    expect_equal(nrow(overview()), 2)

    session$setInputs(delete_row = 0)
    expect_equal(nrow(overview()), 2)

    session$setInputs(delete_row = 1)
    session$setInputs(delete_row = 1.0001)  # a different input value that rounds to the first row
    expect_equal(nrow(overview()), 0)

    session$setInputs(delete_row = 3)  # nothing left to delete
    expect_equal(nrow(overview()), 0)
    expect_named(overview(), c("dataset", "view", "year", "polygon", "wkt", "polygon_sf", "date_from", "date_to"))
  })
})

test_that("clearing the overview keeps the selected polygons", {
  with_server({
    selected_polygons(selected_polygon())
    overview(three_rows())

    session$setInputs(clear_overview = 1)
    expect_equal(nrow(overview()), 0)
    expect_equal(nrow(selected_polygons()), 1)
  })
})

test_that("the download button only appears once something is in the overview", {
  with_server({
    expect_match(as.character(output$download_ui$html), "Download button will appear here")

    overview(three_rows())
    session$flushReact()
    html <- as.character(output$download_ui$html)
    expect_match(html, "check_and_download", fixed = TRUE)
    expect_match(html, "download_data", fixed = TRUE)
    expect_no_match(html, "return_to_r")  # only offered in interactive sessions
  })
})

test_that("downloading builds the zip once, checks it, and serves it", {
  local_webmock()
  webmockr::stub_request("get", uri_regex = adc_re("soiltypes")) |>
    webmockr::to_return(body = json_body(geojson_features(1:3, "soiltype")), headers = json_header)

  with_server({
    overview(overview_row("Soil map"))
    expect_null(prepared_zip())

    session$setInputs(check_and_download = 1)
    zip <- prepared_zip()
    expect_true(file.exists(zip))
    expect_setequal(utils::unzip(zip, list = TRUE)$Name,
                    c("soil_map_geodata_own_polygon.gpkg", "own_polygon.gpkg", "download_summary.csv"))

    served <- output$download_data  # the path of the file that would be sent
    expect_true(file.exists(served))
    expect_setequal(utils::unzip(served, list = TRUE)$Name, utils::unzip(zip, list = TRUE)$Name)

    # a second download replaces the first zip
    session$setInputs(check_and_download = 2)
    expect_false(file.exists(zip))
    expect_true(file.exists(prepared_zip()))

    # the zip is removed when the session ends
    last <- prepared_zip()
    session$close()
    expect_false(file.exists(last))
  })
})

test_that("nothing is offered for download when no dataset could be retrieved", {
  local_mocked_bindings(get_landuse_raster = function(...) stop("nothing here", call. = FALSE), .package = "rNDC")

  with_server({
    overview(overview_row("Land Use", year = 2024L))
    session$setInputs(check_and_download = 1)
    expect_null(prepared_zip())
  })
})
