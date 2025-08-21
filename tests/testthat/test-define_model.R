test_that("model string creation works", {
  filepath <- create_model_string()
  expect_equal(file.exists(filepath), TRUE)
})
