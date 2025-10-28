test_that("test plots for the multivariate model", {
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
    seed = 42,
    univariate = TRUE,
    variances = lambda$variances,
    sds = lambda$variances
  )

  plots <- plot_covariates(full_output, c("x", "y", "z"))
  expect_equal(length(plots), 3)
  for (n in names(plots)) {
    expect_equal(class(plots[[n]])[1], "ggplot2::ggplot")
  }
})