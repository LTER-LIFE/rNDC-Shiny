# The overview table ("shopping cart"): listing, deleting and clearing. (R/server_overview.R)

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
