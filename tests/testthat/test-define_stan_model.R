test_that("model string creation works", {
  filepath <- create_stan_model_string()
  expect_equal(file.exists(filepath), TRUE)
})
