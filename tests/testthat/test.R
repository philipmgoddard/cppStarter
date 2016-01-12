library(cppStarter)
context("Simple tests")

test_that("maxRcpp provides same results as base R max", {
  expect_equal(maxRcpp(c(1, 4, 6, 2, 5)), max(c(1, 4, 6, 2, 5)))
  expect_equal(maxRcpp(c(1, 4, 6, 2, 5), na_rm = TRUE), max(c(1, 4, 6, 2, 5), na.rm = TRUE))
  expect_equal(maxRcpp(c(1, 4, 6, 2, 5, NA)), max(c(1, 4, 6, 2, 5, NA)))
  expect_equal(maxRcpp(c(1, 4, 6, 2, 5, NA), na_rm = TRUE), max(c(1, 4, 6, 2, 5, NA), na.rm = TRUE))
  expect_equal(maxRcpp(c(NA, 1, 4, 6, 2, 5, NA)), max(c(NA, 1, 4, 6, 2, 5, NA)))
  expect_equal(maxRcpp(c(NA, 1, 4, 6, 2, 5, NA), na_rm = TRUE), max(c(NA, 1, 4, 6, 2, 5, NA), na.rm = TRUE))
  expect_equal(maxRcpp(c(NA)), max(c(NA)))
  expect_equal(maxRcpp(c(NA), na_rm = TRUE), max(c(NA), na.rm = TRUE))
})

test_that("minRcpp provides same results as base R min", {
  expect_equal(minRcpp(c(1, 4, 6, 2, 5)), min(c(1, 4, 6, 2, 5)))
  expect_equal(minRcpp(c(1, 4, 6, 2, 5), na_rm = TRUE), min(c(1, 4, 6, 2, 5), na.rm = TRUE))
  expect_equal(minRcpp(c(1, 4, 6, 2, 5, NA)), min(c(1, 4, 6, 2, 5, NA)))
  expect_equal(minRcpp(c(1, 4, 6, 2, 5, NA), na_rm = TRUE), min(c(1, 4, 6, 2, 5, NA), na.rm = TRUE))
  expect_equal(minRcpp(c(NA, 1, 4, 6, 2, 5, NA)), min(c(NA, 1, 4, 6, 2, 5, NA)))
  expect_equal(minRcpp(c(NA, 1, 4, 6, 2, 5, NA), na_rm = TRUE), min(c(NA, 1, 4, 6, 2, 5, NA), na.rm = TRUE))
  expect_equal(minRcpp(c(NA)), min(c(NA)))
  expect_equal(minRcpp(c(NA), na_rm = TRUE), min(c(NA), na.rm = TRUE))
})
