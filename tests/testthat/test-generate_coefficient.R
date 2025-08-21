test_that("error for CI greater than 1", {
  expect_error(generate_coefficient()(10, 0.3, 0.7, 10))
})

test_that("error for CI less than 0", {
  expect_error(generate_coefficient(10, 0.3, 0.7, -1))
})

test_that("error for upper bound less than 0", {
  expect_error(generate_coefficient(10, 0.3, -1, 0.95))
})

test_that("error for upper bound greater than 1", {
  expect_error(generate_coefficient(10, 0.3, 1.1, 0.95))
})

test_that("error for lower bound less than 0", {
  expect_error(generate_coefficient(10, -1, 0.3, 0.95))
})

test_that("error for lower bound greater than 1", {
  expect_error(generate_coefficient(10, 1.1, 0.3, 0.95))
})

test_that("error for lower bound less than 0", {
  expect_error(generate_coefficient(10, -1, 0.3, 0.95))
})

test_that("error samples less than 0", {
  expect_error(generate_coefficient(-1, 0.3, 0.7, 0.95))
})

test_that("error lower bound greater than upper bound", {
  expect_error(generate_coefficient(100, 0.7, 0.3, 0.95))
})

test_that("test that generating the coefficient works`1", {
  set.seed(1234)
  reps <- c()
  for (i in 1:1000) {
    reps <- append(reps, generate_coefficient(10000, 0.3, 0.8, 0.95, 1234))
  }
  expect_equal(
    mean(reps),
    0.6077,
    tolerance = 1e-3
  )
})
