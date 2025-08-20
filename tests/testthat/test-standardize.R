test_that("error for non-vector", {
  expect_error(standardize_with_return(10))
})

test_that("error for list", {
  expect_error(standardize_with_return(list(x = 10)))
  expect_error(standardize_with_return(list(x = c(10, 11))))
})

test_that("error for vector that does not have numeric values", {
  expect_error(standardize_with_return(c("1", "2", "3")))
})

test_that("function works", {
  expected <- list(
    std_data = c(-1.2649111, -0.6324555, 0, 0.6324555, 1.2649111),
    mean_orig = 3,
    sd_orig = 1.581139
  )
  expect_equal(standardize_with_return(c(1, 2, 3, 4, 5)),
    expected,
    tolerance = 1e-5
  )
})
