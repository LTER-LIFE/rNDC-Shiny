# "Which datasets exist for the selected area?" (R/catalogue.R, R/server_catalogue.R)

available_table <- function() {
  tibble::tibble(
    id = c("ahn", "lgn", "ntot", "ndvi"),
    title = c("Elevation (AHN)", "Land Use (LGN)", NA, "NDVI"),
    source = c("AgroDataCube", "NatureDataCube", "NatureDataCube", "NatureDataCube"),
    available = c(FALSE, TRUE, NA, TRUE),
    verified = c(FALSE, TRUE, TRUE, TRUE),
    n_items = c(NA, 1, NA, 12345),
    start = as.POSIXct(c(NA, "2024-01-01", NA, "2017-05-26"), tz = "UTC"),
    end = as.POSIXct(c(NA, "2024-12-31", NA, NA), tz = "UTC"),
    license = c(NA, "CC-BY-4.0", NA, "CC-BY-4.0"), doi = NA_character_, note = NA_character_
  )
}

test_that("the status says yes, no or unknown, and that a dataset of another service is by its known coverage", {
  expect_equal(catalogue_status(c(TRUE, FALSE, NA, TRUE, FALSE), c(TRUE, TRUE, TRUE, FALSE, FALSE)),
               c("✓ yes", "✗ no", "? unknown", "✓ yes (expected)", "✗ no (outside its coverage)"))
})

test_that("the period is told in days, from a start, or not at all", {
  utc <- function(x) as.POSIXct(x, tz = "UTC")
  expect_equal(catalogue_period(utc(c("2024-01-01", "2017-05-26", NA)), utc(c("2024-12-31", NA, NA))),
               c("2024-01-01 to 2024-12-31", "from 2017-05-26", ""))
  expect_equal(catalogue_period(as.Date("1970-01-01"), as.Date(NA)), "from 1970-01-01")
})

test_that("the table puts the datasets with data first, then the unknown, then the others", {
  d <- catalogue_display(available_table())
  expect_named(d, c("Dataset", "Source", "Data for the area", "Items", "Period", "License"))
  expect_equal(d$Dataset, c("Land Use (LGN)", "NDVI", "ntot", "Elevation (AHN)"))   # no title: the id
  expect_equal(d$`Data for the area`[1:3], c("✓ yes", "✓ yes", "? unknown"))
  expect_equal(d$Items, c("1", "12,345", "", ""))
  expect_equal(d$Period[1:2], c("2024-01-01 to 2024-12-31", "from 2017-05-26"))
  expect_equal(nrow(catalogue_display(NULL)), 0)
  expect_equal(nrow(catalogue_display(available_table()[0, ])), 0)
})

test_that("the note counts the datasets, and says what the datasets of other services stand on", {
  note <- catalogue_note(available_table())
  expect_match(note, "^2 of 4 datasets have data for the selected area\\.")
  expect_match(note, "known for them: nothing is asked of those services", fixed = TRUE)
  expect_match(catalogue_note(available_table(), c("2024-06-01", "2024-06-30")), "in the period 2024-06-01 to 2024-06-30", fixed = TRUE)
  expect_false(grepl("other services", catalogue_note(available_table()[2:3, ])))
})

test_that("the dialog lists the datasets of the selected area, from the STAC API and the known coverage", {
  local_clean_cache()
  mock_check()
  item <- raster_item("a", "2024-01-01")
  item$collection <- "lgn"
  local_stac_api(pages = list(list(item)))

  with_server({
    selected_polygons(selected_polygon())
    session$setInputs(check_availability = 1)
    html <- as.character(output$catalogue_table)
    expect_match(html, "lgn", fixed = TRUE)
    expect_match(html, "✓ yes", fixed = TRUE)
    expect_match(html, "Weather (KNMI daily observations)", fixed = TRUE)
    expect_match(html, "✓ yes (expected)", fixed = TRUE)         # inside the Netherlands
    expect_match(html, "✗ no", fixed = TRUE)                     # a collection without items
    expect_match(as.character(output$catalogue_note$html), "datasets have data for the selected area")
    expect_null(last_request_body()$datetime)                          # no period asked for
  })
})

test_that("the period is passed on when the user asks for one, and each question is cached", {
  local_clean_cache()
  mock_check()
  local_stac_api(pages = list(list()))

  with_server({
    selected_polygons(selected_polygon())
    session$setInputs(check_availability = 1, catalogue_limit = TRUE,
                      catalogue_period = as.Date(c("2024-06-01", "2024-06-30")))
    invisible(output$catalogue_table)
    expect_equal(last_request_body()$datetime, "2024-06-01T00:00:00Z/2024-06-30T00:00:00Z")
    expect_match(as.character(output$catalogue_note$html), "in the period 2024-06-01 to 2024-06-30", fixed = TRUE)

    n <- length(request_uris())
    session$setInputs(catalogue_limit = TRUE)                          # the same question again
    invisible(output$catalogue_table)
    expect_length(request_uris(), n)

    session$setInputs(catalogue_limit = FALSE)                         # another: without the period
    invisible(output$catalogue_table)
    expect_null(last_request_body()$datetime)
  })
})

test_that("the dialog says to select an area first, and why the datasets could not be listed", {
  local_clean_cache()
  mock_check()
  local_stac_api(pages = list(list()), status = 500L)

  with_server({
    session$setInputs(check_availability = 1)
    expect_match(as.character(output$catalogue_note$html), "Select an area on the map first.", fixed = TRUE)

    selected_polygons(selected_polygon())
    session$flushReact()
    expect_match(as.character(output$catalogue_note$html), "The datasets could not be listed", fixed = TRUE)
    expect_equal(nrow(catalogue_display(NULL)), 0)
  })
})

test_that("text from the services is not read as HTML", {
  local_clean_cache()
  mock_check()
  evil <- ref_collection("lgn")
  evil$title <- "<script>alert(1)</script>"
  local_stac_api(pages = list(list()), collections = list(evil))

  with_server({
    selected_polygons(selected_polygon())
    session$setInputs(check_availability = 1)
    html <- as.character(output$catalogue_table)
    expect_false(grepl("<script>", html, fixed = TRUE))
    expect_match(html, "&lt;script&gt;", fixed = TRUE)
  })
})

test_that("the first dates of the datasets of other services are those of rNDC", {
  ext <- rNDC::ndc_external_datasets()
  expect_equal(weather_min_date, ext$start[ext$id == "weather"])
  expect_equal(fields_min_year, 2009L)
  expect_equal(ndvi_min_month, as.Date("2017-05-01"))
})
