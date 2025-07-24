test_that("fisher z transform works", {
  expect_equal(fisher_z_transform(0.30), 0.3095196, tolerance = 1e-5)
  expect_equal(fisher_z_transform(0.80), 1.098612, tolerance = 1e-5)
  expect_equal(fisher_z_transform(0.25), 0.2554128, tolerance = 1e-5)
  expect_equal(fisher_z_transform(0.70), 0.8673005, tolerance = 1e-5)
  expect_equal(fisher_z_transform(0.40), 0.4236489, tolerance = 1e-5)
  expect_equal(fisher_z_transform(0.70), 0.8673005, tolerance = 1e-5)
})

test_that("error for number greater than 1", {
  expect_error(fisher_z_transform(1.5))
})

test_that("error for number less than -1", {
  expect_error(fisher_z_transform(-1.5))
})

test_that("error for non-numeric", {
  expect_error(fisher_z_transform("0.30"))
})
