test_that("test end to end example", {
  set.seed(42)

  n <- 100
  a1 <- 3
  a2 <- 4
  a3 <- 5
  shape1 <- 2
  rate1 <- 2
  shape2 <- 3
  rate2 <- 3
  shape3 <- 5
  rate3 <- 5

  x <- rgamma(n, shape1, rate1)
  y <- rgamma(n, shape2, rate2)
  z <- rgamma(n, shape3, rate3)
  output <- a1 * x + a2 * y + a3 * z + rnorm(n)

  frame <- data.frame(
    list(
      output = output,
      x = x,
      y = y,
      z = z
    )
  )

  # Create the tainted data and fit a model
  tainted_df <- frame
  tainted_df$x <- frame$x + rnorm(100, mean = 2)
  tainted_df$y <- frame$y + rnorm(100, mean = 2)
  tainted_df$z <- frame$z + rnorm(100, mean = 2)
  tainted_model <- glm("output ~ x + y + z", data = tainted_df)

  # Use the measurement error adjust
  x_v_coef <- generate_coefficient(1000, 0.4, 0.7, 0.95)
  y_v_coef <- generate_coefficient(1000, 0.5, 0.7, 0.95)
  z_v_coef <- generate_coefficient(1000, 0.3, 0.6, 0.95)

  output <- acme_model(tainted_df, c("x", "y", "z"))
  validity_coefficients <- c(x_v_coef, y_v_coef, z_v_coef)
  lambda <- attenuation_matrix(output, c("x", "y", "z"), validity_coefficients)
  model_output <- multivariate_model(
    "output ~ x + y + z",
    data = tainted_df,
    columns = c("x", "y", "z"),
    a_c_matrix = lambda$matrix,
    sds = lambda$sds,
    variances = lambda$variances,
    univariate = TRUE
  )

  expect_lt(a1, quantile(model_output$multivariate$x, 0.975))
  expect_lt(a2, quantile(model_output$multivariate$y, 0.975))
  expect_lt(a3, quantile(model_output$multivariate$z, 0.975))
})
