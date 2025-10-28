test_that("test sensitivity analysis", {
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
  params <- list(
    x = c(0.1),
    y = c(0.1),
    z = c(0.1)
  )

  output_jags <- sensitivity_analysis(
    params,
    df,
    "output ~ x + y + z",
    c("x", "y", "z"),
    seed = 1234
  )

  expect_equal(
    as.numeric(output_jags[["means"]][["x"]]),
    -17.67057,
    tolerance = 0.1
  )
  expect_equal(
    as.numeric(output_jags[["means"]][["y"]]),
    0.6642302,
    tolerance = 0.1
  )
  expect_equal(
    as.numeric(output_jags[["means"]][["z"]]),
    5.395354,
    tolerance = 0.1
  )

  expect_equal(
    as.numeric(output_jags[["sds"]][["x"]]),
    11.1923,
    tolerance = 0.1
  )
  expect_equal(
    as.numeric(output_jags[["sds"]][["y"]]),
    11.83524,
    tolerance = 0.1
  )
  expect_equal(
    as.numeric(output_jags[["sds"]][["z"]]),
    10.31778,
    tolerance = 0.1
  )
})

test_that("test sensitivity analysis (stan backend)", {
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
  params <- list(
    x = c(0.1),
    y = c(0.1),
    z = c(0.1)
  )

  output_stan <- sensitivity_analysis(
    params,
    df,
    "output ~ x + y + z",
    c("x", "y", "z"),
    seed = 1234,
    stan = TRUE
  )

  expect_equal(
    as.numeric(output_stan[["means"]][["x"]]),
    -17.67057,
    tolerance = 0.1
  )
  expect_equal(
    as.numeric(output_stan[["means"]][["y"]]),
    0.6642302,
    tolerance = 0.1
  )
  expect_equal(
    as.numeric(output_stan[["means"]][["z"]]),
    5.395354,
    tolerance = 0.1
  )

  expect_equal(
    as.numeric(output_stan[["sds"]][["x"]]),
    11.1923,
    tolerance = 0.1
  )
  expect_equal(
    as.numeric(output_stan[["sds"]][["y"]]),
    11.83524,
    tolerance = 0.1
  )
  expect_equal(
    as.numeric(output_stan[["sds"]][["z"]]),
    10.31778,
    tolerance = 0.1
  )
})