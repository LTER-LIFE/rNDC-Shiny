# The check next to "Add to overview": the checks of adding a dataset, for the settings above, without adding it; and Add to
# overview, which is enabled only for the settings that were checked. (R/server_dataset.R)

second_polygon <- function() {
  second <- selected_polygon("Area B")
  sf::st_geometry(second) <- sf::st_geometry(fixed_square(2, 5.9, 52.2))
  second$wkt <- sf::st_as_text(sf::st_geometry(second))
  second
}

test_that("the two buttons are next to each other, and both start disabled", {
  html <- as.character(app_ui())
  tags_of <- function(id) regmatches(html, regexpr(sprintf('<button[^>]*id="%s"[^>]*>[^<]*</button>', id), html))
  expect_match(tags_of("check_dataset"), ">Check data availability<", fixed = TRUE)
  expect_match(tags_of("check_dataset"), " disabled ", fixed = TRUE)
  expect_match(tags_of("add_dataset"), ">Add to overview<", fixed = TRUE)
  expect_match(tags_of("add_dataset"), " disabled ", fixed = TRUE)
  expect_lt(regexpr('id="check_dataset"', html, fixed = TRUE), regexpr('id="add_dataset"', html, fixed = TRUE))
  between <- substr(html, regexpr('id="check_dataset"', html, fixed = TRUE), regexpr('id="add_dataset"', html, fixed = TRUE))
  expect_false(grepl("<div", between, fixed = TRUE))                    # in the same row
})

test_that("Add to overview waits for the check: it checks the dataset for the settings, and adds nothing", {
  local_clean_cache()
  rec <- mock_check("yes")
  notes <- mock_notes()
  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    expect_false(check_dataset_enabled())                          # no area yet
    selected_polygons(selected_polygon())
    session$flushReact()
    expect_true(check_dataset_enabled())
    expect_false(add_enabled())                                    # not before the check

    session$setInputs(check_dataset = 1)
    expect_equal(nrow(overview()), 0)                              # (the check adds nothing)
    expect_length(rec$calls, 1)
    expect_equal(rec$calls[[1]]$datasets, "Land Use")
    expect_equal(rec$calls[[1]]$years, list("Land Use" = 2024L))
    expect_true(add_enabled())
    expect_equal(notes$notes[[length(notes$notes)]][[2]], "message")
    expect_match(notes$notes[[length(notes$notes)]][[1]], "Checked: Land Use can be added for the settings above.", fixed = TRUE)
    expect_equal(availability()$dataset, "Land Use")               # and the menu shows what was found

    session$setInputs(add_dataset = 1)
    expect_equal(overview()$dataset, "Land Use")
    expect_true(add_enabled())                                     # (the settings are the same, so are the checks)
  })
})

test_that("a change of year, date, months, view, dataset, Weather distance or area disables Add until it is checked again", {
  local_clean_cache()
  mock_check("yes")
  with_server({
    selected_polygons(selected_polygon())
    checked <- function(n) {
      session$setInputs(check_dataset = n)
      expect_true(add_enabled())
    }
    n <- 0
    next_check <- function() { n <<- n + 1; checked(n) }

    # the year
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    next_check()
    session$setInputs(landuse_year = "2023")
    expect_false(add_enabled())
    expect_true(check_dataset_enabled())                           # (it can be checked)
    next_check()

    # the dataset: also back to one that was checked
    session$setInputs(selected_dataset = "Nitrogen", nitrogen_year = "2025")
    expect_false(add_enabled())
    next_check()
    session$setInputs(selected_dataset = "Land Use")
    expect_false(add_enabled())

    # a single month or a range of months, and the months
    session$setInputs(selected_dataset = "NDVI", ndvi_mode = "single", ndvi_year = 2024, ndvi_month = 6, ndvi_tab = "Geodata")
    next_check()
    session$setInputs(ndvi_month = 7)
    expect_false(add_enabled())
    next_check()
    session$setInputs(ndvi_mode = "range", ndvi_from_year = 2024, ndvi_from_month = 1, ndvi_to_year = 2024, ndvi_to_month = 6)
    expect_false(add_enabled())
    next_check()
    session$setInputs(ndvi_to_month = 8)
    expect_false(add_enabled())
    next_check()

    # the view (tab)
    session$setInputs(ndvi_tab = "Statistics")
    expect_false(add_enabled())
    next_check()

    # Weather: the date, the period and the distance
    session$setInputs(selected_dataset = "Weather", weather_mode = "single", weather_date = as.Date("2025-06-01"))
    next_check()
    session$setInputs(weather_date = as.Date("2025-06-02"))
    expect_false(add_enabled())
    next_check()
    session$setInputs(weather_max_km = 60)
    expect_false(add_enabled())
    next_check()
    session$setInputs(weather_mode = "period", weather_period = as.Date(c("2025-06-01", "2025-06-10")))
    expect_false(add_enabled())
    next_check()

    # the area (the same shape under another name is the same area)
    selected_polygons(selected_polygon("Other polygon"))
    session$flushReact()
    expect_true(add_enabled())
    selected_polygons(second_polygon())
    session$flushReact()
    expect_false(add_enabled())
    next_check()
    selected_polygons(NULL)
    session$flushReact()
    expect_false(add_enabled())
    expect_false(check_dataset_enabled())
  })
})

test_that("a dataset without data for the polygon does not pass the check, and says what to change", {
  local_clean_cache()
  mock_check("no")
  notes <- mock_notes()
  with_server({
    session$setInputs(selected_dataset = "AHN")
    selected_polygons(selected_polygon())
    session$setInputs(check_dataset = 1)
    expect_false(add_enabled())
    expect_null(check_passed())
    last <- notes$notes[[length(notes$notes)]]
    expect_equal(last[[2]], "error")
    expect_match(last[[1]], "has no data for the selected area (Own polygon). Nothing can be added: change the area, the year or the period, and try again.", fixed = TRUE)
    expect_equal(nrow(overview()), 0)
  })
})

test_that("with several polygons the check passes when some have data, and says which ones are left out", {
  local_clean_cache()
  mock_check(list("yes", "no", "yes", "no"))      # the check, then the check of adding
  notes <- mock_notes()
  with_server({
    session$setInputs(selected_dataset = "Soil map")
    selected_polygons(rbind(selected_polygon("Area A"), second_polygon()))
    session$setInputs(check_dataset = 1)
    expect_true(add_enabled())
    warning <- Filter(function(x) x[[2]] == "warning", notes$notes)
    expect_match(warning[[length(warning)]][[1]], "No Soil map data was found for Area B: the dataset will be added for the other polygons only.", fixed = TRUE)
    expect_equal(availability()$status, "partial")

    session$setInputs(add_dataset = 1)
    expect_equal(overview()$polygon, "Area A")
  })
})

test_that("a check that could not be made does not stop the dataset from being added", {
  local_clean_cache()
  mock_check(list(NULL))
  with_server({
    session$setInputs(selected_dataset = "Land Use", landuse_year = "2024")
    selected_polygons(selected_polygon())
    session$setInputs(check_dataset = 1)
    expect_true(add_enabled())
  })
})

test_that("the settings are verified like when adding, and the time of the retrieval is checked", {
  local_clean_cache()
  rec <- mock_check("yes")
  notes <- mock_notes()
  with_server({
    selected_polygons(selected_polygon())
    # an NDVI month in the future
    session$setInputs(selected_dataset = "NDVI", ndvi_mode = "single", ndvi_year = as.integer(format(Sys.Date(), "%Y")) + 2,
                      ndvi_month = 1)
    session$setInputs(check_dataset = 1)
    expect_equal(notes$notes[[length(notes$notes)]][[2]], "error")
    expect_match(notes$notes[[length(notes$notes)]][[1]], "in the future", fixed = TRUE)
    expect_false(add_enabled())
    expect_length(rec$calls, 0)                                    # (nothing was asked of the services)

    # more than the time that is allowed
    session$setInputs(ndvi_year = 2024, ndvi_month = 6)
    local_mocked_bindings(retrieve_estimate_seconds = function(...) rep(1e6, 2)[seq_along(list(...)[[1]])])
    session$setInputs(check_dataset = 2)
    expect_match(notes$notes[[length(notes$notes)]][[1]], "The overview would take", fixed = TRUE)
    expect_false(add_enabled())
  })
})
