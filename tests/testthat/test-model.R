test_that("error is thrown when not a data.frame", {
  expect_error(acme_model(c(1, 2, 3, 4)))
  expect_error(acme_model(list(x = c(1, 2, 3, 4))))
})

test_that("error is thrown when not numeric", {
  df <- data.frame(
    list(x = c(1, 2, 3, 4), y = c(2, 3, 4, 5))
  )
  expect_error(acme_model(df, c("x", "y"), n_chains = "1"))
  expect_error(acme_model(df, c("x", "y"), n_adapt_steps = "1"))
  expect_error(acme_model(df, c("x", "y"), n_burn = "1"))
  expect_error(acme_model(df, c("x", "y"), n_thin = "1"))
  expect_error(acme_model(df, c("x", "y"), n_steps = "1"))
  expect_error(acme_model(df, c("x", "y"), seed = "1"))
})

test_that("error is thrown when irregular values specified", {
  df <- data.frame(
    list(x = c(1, 2, 3, 4), y = c(2, 3, 4, 5))
  )
  expect_error(acme_model(df, c("x", "y"), n_chains = 0))
  expect_error(acme_model(df, c("x", "y"), n_chains = 10))
  expect_error(acme_model(df, c("x", "y"), n_adapt_steps = 0))
  expect_error(acme_model(df, c("x", "y"), n_burn = 0))
  expect_error(acme_model(df, c("x", "y"), n_thin = 0))
  expect_error(acme_model(df, c("x", "y"), n_thin = 10))
  expect_error(acme_model(df, c("x", "y"), n_steps = 0))
})

test_that("error is when columns don't match", {
  df <- data.frame(
    list(x = c(1, 2, 3, 4), y = c(2, 3, 4, 5))
  )
  expect_error(acme_model(df, c("x", "y", "z")))
})

test_that("acme modelling works", {
  df <- data.frame(
    list(x = c(1, 2, 3, 4), y = c(2, 3, 4, 5))
  )
  acme_model(df, names(df))
})

test_that("acme modelling correct output type", {
  df <- data.frame(
    list(x = c(1, 2, 3, 4), y = c(2, 3, 4, 5))
  )
  expect_equal(
    is.list(acme_model(df, names(df))),
    TRUE
  )
})

test_that("acme modelling correct output", {
  df <- data.frame(
    list(
      x = c(1, 2, 3, 4),
      y = c(2, 3, 4, 5),
      z = c(3, 4, 5, 6)
    )
  )
  output <- acme_model(df, names(df), seed = 42)
  means <- colMeans(data.frame(output$samples[[1]]))

  # test mean value of samples
  expect_equal(
    as.vector(means[1]),
    1
  )
  expect_equal(
    as.vector(means[2]),
    0.7164143,
    tolerance = 0.1
  )
  expect_equal(
    as.vector(means[3]),
    0.7177638,
    tolerance = 0.1
  )
  expect_equal(
    as.vector(means[4]),
    0.7164143,
    tolerance = 0.1
  )
  expect_equal(
    as.vector(means[5]),
    1
  )
  expect_equal(
    as.vector(means[6]),
    0.7189397,
    tolerance = 0.1
  )
  expect_equal(
    as.vector(means[7]),
    0.7177638,
    tolerance = 0.1
  )
  expect_equal(
    as.vector(means[8]),
    0.7189397,
    tolerance = 0.1
  )
  expect_equal(
    as.vector(means[9]),
    1
  )
  expect_equal(
    as.vector(means[10]),
    1.3712500,
    tolerance = 0.1
  )
  expect_equal(
    as.vector(means[11]),
    1.3687147,
    tolerance = 0.1
  )
  expect_equal(
    as.vector(means[12]),
    1.3717495,
    tolerance = 0.1
  )
})
