test_that("error thrown when not a list", {
  expect_error(attenuation_matrix(c(1, 2, 3, 4), c("1"), c(1)))
})

test_that("error thrown when columns don't match validity coefficients", {
  expect_error(attenuation_matrix(
    list(x = c(1, 2, 3, 4)),
    c("1"),
    c(1, 2)
  ))
})
