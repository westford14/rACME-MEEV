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

test_that("error thrown only two values", {
  expect_error(attenuation_matrix(
    list(x = c(1, 2, 3, 4), y = c(1, 2, 3, 4)),
    c("x", "y"),
    c(0.5, 0.5)
  ))
})

test_that("function works in conjunction with acme_model", {
  df <- data.frame(
    list(
      x = c(1, 2, 3, 4),
      y = c(2, 3, 4, 5),
      z = c(3, 4, 5, 6)
    )
  )
  output <- acme_model(df, names(df))
  attenuation_matrix(
    output,
    names(df),
    c(0.5, 0.5, 0.5)
  )
  expect_equal(TRUE, TRUE)
})

test_that("test matrix output", {
  df <- data.frame(
    list(
      x = c(1, 2, 3, 4),
      y = c(2, 3, 4, 5),
      z = c(3, 4, 5, 6)
    )
  )
  output <- acme_model(df, names(df))
  expect_equal(
    is.list(attenuation_matrix(
      output,
      names(df),
      c(0.5, 0.5, 0.5)
    )),
    TRUE
  )
})

test_that("test matrix output values", {
  df <- data.frame(
    list(
      x = c(1, 2, 3, 4),
      y = c(2, 3, 4, 5),
      z = c(3, 4, 5, 6)
    )
  )
  output <- acme_model(df, names(df), seed = 42)
  temp <- attenuation_matrix(
    output,
    names(df),
    c(0.5, 0.5, 0.5)
  )

  # testing standard deviations
  expect_equal(
    temp$sds$x,
    1.368157,
    tolerance = 0.1
  )
  expect_equal(
    temp$sds$y,
    1.370803,
    tolerance = 0.1
  )
  expect_equal(
    temp$sds$z,
    1.374033,
    tolerance = 0.1
  )

  # testing variances
  expect_equal(
    temp$variances$x,
    0.4679634,
    tolerance = 0.1
  )
  expect_equal(
    temp$variances$y,
    0.4697754,
    tolerance = 0.1
  )
  expect_equal(
    temp$variances$z,
    0.4719917,
    tolerance = 0.1
  )
})

test_that("test matrix output values (stan backend)", {
  skip_if(Sys.getenv("CI") == "true", "only run the Stan tests locally")
  skip_on_covr()
  df <- data.frame(
    list(
      x = c(1, 2, 3, 4),
      y = c(2, 3, 4, 5),
      z = c(3, 4, 5, 6)
    )
  )
  output <- acme_model(df, names(df), seed = 42, stan = TRUE)
  temp <- attenuation_matrix(
    output,
    names(df),
    c(0.5, 0.5, 0.5),
    stan = TRUE
  )

  # testing standard deviations
  expect_equal(
    temp$sds$x,
    1.076613,
    tolerance = 0.1
  )
  expect_equal(
    temp$sds$y,
    1.07806,
    tolerance = 0.1
  )
  expect_equal(
    temp$sds$z,
    1.069811,
    tolerance = 0.1
  )

  # testing variances
  expect_equal(
    temp$variances$x,
    0.289774,
    tolerance = 0.5
  )
  expect_equal(
    temp$variances$y,
    0.2905532,
    tolerance = 0.1
  )
  expect_equal(
    temp$variances$z,
    0.286124,
    tolerance = 0.1
  )
})
