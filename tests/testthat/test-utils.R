# Shared helpers. (R/utils.R)

test_that("safe_filename and same_na", {
  expect_equal(safe_filename("a b/c.d"), "a_b_c_d")
  expect_equal(safe_filename("Light on Nature_1"), "Light_on_Nature_1")
  expect_true(same_na(NA, NA))
  expect_true(same_na(2024L, 2024L))
  expect_false(same_na(NA, 2024L))
  expect_false(same_na(2024L, 2025L))
})
