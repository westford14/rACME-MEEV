test_that("multiplication works", {
  filepath <- create_model_string()
  expect_equal(file.exists(filepath), TRUE)
})
