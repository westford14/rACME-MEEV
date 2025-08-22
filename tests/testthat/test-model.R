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
