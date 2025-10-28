test_that("test traceplots for the multivariate model", {
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

  plots <- traceplots(full_output$naive, c("x", "y", "z"))
  expect_equal(length(plots), 3)
  expect_equal(class(plots)[1], "patchwork")
  expect_equal(class(plots)[2], "ggplot2::ggplot")
})

test_that("test traceplots for the pre-model", {
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

  plots <- traceplots(output$samples, c("x", "y", "z"), pre_model = TRUE)
  expect_equal(length(plots), 3)
  expect_equal(class(plots)[1], "patchwork")
  expect_equal(class(plots)[2], "ggplot2::ggplot")
})

test_that("test traceplots for the multivariate model (stan backend)", {
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
  output <- acme_model(df, c("x", "y", "z"), seed = 42, stan = TRUE)
  lambda <- attenuation_matrix(
    output,
    c("x", "y", "z"), c(0.5, 0.5, 0.5),
    stan = TRUE
  )
  full_output <- multivariate_model(
    "output ~ x + y + z",
    df,
    c("x", "y", "z"),
    lambda$matrix,
    seed = 42
  )

  plots <- traceplots(full_output$naive, c("x", "y", "z"), stan = TRUE)
  expect_equal(length(plots), 3)
  expect_equal(class(plots)[1], "patchwork")
  expect_equal(class(plots)[2], "ggplot2::ggplot")
})

test_that("test traceplots for the pre-model (stan backend)", {
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
  output <- acme_model(df, c("x", "y", "z"), seed = 42, stan = TRUE)

  plots <- traceplots(
    output$samples, c("x", "y", "z"),
    pre_model = TRUE,
    stan = TRUE
  )
  expect_equal(length(plots), 3)
  expect_equal(class(plots)[1], "patchwork")
  expect_equal(class(plots)[2], "ggplot2::ggplot")
})