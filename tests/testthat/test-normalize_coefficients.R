test_that("error for CI greater than 1", {
  expect_error(normalize_coefficients(10, 0.3, 0.7, 10))
})

test_that("error for CI less than 0", {
  expect_error(normalize_coefficients(10, 0.3, 0.7, -1))
})

test_that("error for upper bound less than 0", {
  expect_error(normalize_coefficients(10, 0.3, -1, 0.95))
})

test_that("error for upper bound greater than 1", {
  expect_error(normalize_coefficients(10, 0.3, 1.1, 0.95))
})

test_that("error for lower bound less than 0", {
  expect_error(normalize_coefficients(10, -1, 0.3, 0.95))
})

test_that("error for lower bound greater than 1", {
  expect_error(normalize_coefficients(10, 1.1, 0.3, 0.95))
})

test_that("error for lower bound less than 0", {
  expect_error(normalize_coefficients(10, -1, 0.3, 0.95))
})

test_that("error samples less than 0", {
  expect_error(normalize_coefficients(-1, 0.3, 0.7, 0.95))
})

test_that("error lower bound greater than upper bound", {
  expect_error(normalize_coefficients(100, 0.7, 0.3, 0.95))
})
