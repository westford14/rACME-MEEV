test_that("test that plots are returned", {
  df <- data.frame(
    dose = c("D0.5", "D1", "D2"),
    len = c(4.2, 10, 29.5)
  )
  a <- ggplot(data = df, aes(x = dose, y = len, group = 1)) +
    geom_line() +
    geom_point()
  b <- ggplot(data = df, aes(x = dose, y = len, group = 1)) +
    geom_line() +
    geom_point()
  plots <- plot_a_list(list(a = a, b = b), rows = 1, columns = 2)

  expect_equal(class(plots)[1], "patchwork")
  expect_equal(length(plots), 2)
})