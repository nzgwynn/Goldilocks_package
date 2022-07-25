test_that("the ordering for the interactive plot works", {

  df <- data.frame(x = seq(10, 5, length = 10), y = seq(30, 50, length = 10))
  result_x <- make.order(column = "x", data = df)
  result_y <- make.order(column = "y", data = df)

  expect_equal(seq(5, 10, length = 10), result_x$x)
  expect_equal(seq(50, 30, length = 10), result_x$y)

  expect_equal(seq(10, 5, length = 10), result_y$x)
  expect_equal(seq(30, 50, length = 10), result_y$y)

  expect_true(is.factor(result_x$G))
})
