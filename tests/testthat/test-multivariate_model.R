test_that("error is thrown when not a data.frame", {
  expect_error(multivariate_model("y ~ x", c(1, 2, 3, 4)), NULL, NULL)
})
