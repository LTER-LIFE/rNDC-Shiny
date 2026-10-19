# The constants and the helpers of the dataset menu. (R/constants.R)

test_that("dataset and tab helpers", {
  expect_equal(dataset_tab_id("Land Use"), "land_use_tab")
  expect_equal(dataset_target_id("Soil map", "Geodata"), "tab_target_soil_map_geodata")
  expect_equal(dataset_tabs("NDVI"), c("Statistics", "Geodata"))
  expect_equal(dataset_tabs("Weather"), "Statistics")
  expect_equal(dataset_tabs("AHN"), c("Statistics", "Geodata"))
  expect_equal(dataset_tabs("Soil map"), "Geodata")
  expect_equal(dataset_tabs("Land Use"), "Geodata")
  expect_equal(dataset_default_tab("NDVI"), "Statistics")
  expect_true(all(adc_datasets %in% all_dataset_names))
  expect_equal(adc_datasets, c("Weather", "Agricultural fields"))   # Soil map and AHN come from the NatureDataCube
})

test_that("rNDC constants are available", {
  expect_equal(landuse_default_year, 2024L)
  expect_equal(nitrogen_layer_choices, c("ntot", "nox", "nh3"))
})
