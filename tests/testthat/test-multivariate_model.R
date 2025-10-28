test_that("error is thrown when not a data.frame", {
  expect_error(
    multivariate_model("y ~ x", c(1, 2, 3, 4), c("1"), matrix(c(1, 2)))
  )
})

test_that("error if univariate and no variances", {
  expect_error(
    multivariate_model(
      "y ~ x",
      data.frame,
      c("1"),
      matrix(c(1, 2)),
      univariate = TRUE,
      sds = list()
    )
  )
})

test_that("error if univariate and no SDs", {
  expect_error(
    multivariate_model(
      "y ~ x",
      data.frame,
      c("1"),
      matrix(c(1, 2)),
      univariate = TRUE,
      variances = list()
    )
  )
})

test_that("error is thrown when formula is not a character", {
  expect_error(multivariate_model(1, data.frame(), c("1"), matrix(c(1, 2))))
})

test_that("error is thrown when columns is not a vector", {
  expect_error(
    multivariate_model("y ~ x", data.frame(), "target", matrix(c(1, 2)))
  )
})

test_that("error is thrown when matrix is not a matrix", {
  expect_error(multivariate_model("y ~ x", data.frame(), c("x"), data.frame()))
})

test_that("multivariate model works", {
  set.seed(42)
  x <- rnorm(100)
  set.seed(43)
  y <- rnorm(100)
  set.seed(44)
  z <- rnorm(100)
  set.seed(45)
  output <- rnorm(100)

  df <- data.frame(
    list(
      x = x,
      y = y,
      z = z,
      output = output
    )
  )
  output <- acme_model(df, c("x", "y", "z"), seed = 42)
  lambda <- attenuation_matrix(output, c("x", "y", "z"), c(0.5, 0.5, 0.5))
  expect_equal(
    is.list(
      multivariate_model(
        "output ~ x + y + z",
        df,
        c("x", "y", "z"),
        lambda$matrix,
        seed = 42
      )
    ),
    TRUE
  )
})

test_that("multivariate model works with univariate", {
  set.seed(42)
  x <- rnorm(100)
  set.seed(43)
  y <- rnorm(100)
  set.seed(44)
  z <- rnorm(100)
  set.seed(45)
  output <- rnorm(100)

  df <- data.frame(
    list(
      x = x,
      y = y,
      z = z,
      output = output
    )
  )
  output <- acme_model(df, c("x", "y", "z"), seed = 42)
  lambda <- attenuation_matrix(output, c("x", "y", "z"), c(0.5, 0.5, 0.5))
  expect_equal(
    is.list(
      multivariate_model(
        "output ~ x + y + z",
        df,
        c("x", "y", "z"),
        lambda$matrix,
        seed = 42,
        univariate = TRUE,
        sds = lambda[["sds"]],
        variances = lambda[["variances"]]
      )
    ),
    TRUE
  )
})

test_that("expect error when formula misspecified", {
  df <- data.frame(
    list(
      x = c(1, 2, 3, 4),
      y = c(2, 3, 4, 5),
      z = c(3, 4, 5, 6),
      output = c(1, 2, 3, 4)
    )
  )
  output <- acme_model(df, c("x", "y", "z"), seed = 42)
  lambda <- attenuation_matrix(output, c("x", "y", "z"), c(0.5, 0.5, 0.5))
  expect_error(multivariate_model(
    "output ~ x - y - z + f",
    df,
    c("x", "y", "z"),
    lambda$matrix
  ))
})

test_that("test output values", {
  set.seed(42)
  x <- rnorm(100)
  set.seed(43)
  y <- rnorm(100)
  set.seed(44)
  z <- rnorm(100)
  set.seed(45)
  output <- rnorm(100)

  df <- data.frame(
    list(
      x = x,
      y = y,
      z = z,
      output = output
    )
  )
  output <- acme_model(df, c("x", "y", "z"), seed = 42)
  lambda <- attenuation_matrix(output, c("x", "y", "z"), c(0.5, 0.5, 0.5))
  expect_equal(
    is.list(
      multivariate_model(
        "output ~ x + y + z",
        df,
        c("x", "y", "z"),
        lambda$matrix,
        seed = 42
      )
    ),
    TRUE
  )
})

test_that("test output values", {
  set.seed(42)
  x <- rnorm(100)
  set.seed(43)
  y <- rnorm(100)
  set.seed(44)
  z <- rnorm(100)
  set.seed(45)
  output <- rnorm(100)

  df <- data.frame(
    list(
      x = x,
      y = y,
      z = z,
      output = output
    )
  )
  output <- acme_model(df, c("x", "y", "z"), seed = 42)
  lambda <- attenuation_matrix(output, c("x", "y", "z"), c(0.5, 0.5, 0.5))
  full_output <- multivariate_model(
    "output ~ x + y + z",
    df,
    c("x", "y", "z"),
    lambda$matrix,
    seed = 42
  )

  # Test naive values
  expect_equal(
    summary(full_output$naive)$statistics["x", "Mean"],
    -0.1784309,
    tolerance = 0.1
  )
  expect_equal(
    summary(full_output$naive)$statistics["y", "Mean"],
    0.006618242,
    tolerance = 0.1
  )
  expect_equal(
    summary(full_output$naive)$statistics["z", "Mean"],
    0.05446455,
    tolerance = 0.1
  )

  # Test multivariate values
  means <- colMeans(full_output$multivariate)
  expect_equal(
    as.vector(means[1]),
    -0.71372377,
    tolerance = 0.1
  )
  expect_equal(
    as.vector(means[2]),
    0.02647297,
    tolerance = 0.1
  )
  expect_equal(
    as.vector(means[3]),
    0.21785821,
    tolerance = 0.1
  )
})
