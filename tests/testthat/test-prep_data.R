test_that("error is thrown when not a data.frame", {
  expect_error(create_modelling_data(c(1, 2, 3, 4)))
  expect_error(create_modelling_data(list(x = c(1, 2, 3, 4))))
})

test_that("error is when columns don't match", {
  df <- data.frame(
    list(x = c(1, 2, 3, 4), y = c(2, 3, 4, 5))
  )
  expect_error(create_modelling_data(df, c("x", "y", "z")))
})

test_that("expected response", {
  expected <- list(
    data = data.frame(
      x = c(-1.1618950, -0.3872983, 0.3872983, 1.1618950),
      y = c(-1.1618950, -0.3872983, 0.3872983, 1.1618950)
    ),
    n = 4,
    p = 2,
    sd_orig = data.frame(list(x = 1.290994, y = 1.290994)),
    mean_orig = data.frame(list(x = 2.5, y = 3.5)),
    zdf = 2,
    zRmat = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  )
  df <- data.frame(
    list(x = c(1, 2, 3, 4), y = c(2, 3, 4, 5))
  )
  expect_equal(create_modelling_data(df, names(df)),
    expected,
    tolerance = 1e-5
  )
})
