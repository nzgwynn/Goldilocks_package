
test_that("make plot works", {

  data <- data.frame(Var_1 = seq(0, 1, length = 9),
                     Var_2 = seq(0, 5, length = 9),
                     Var_3 = seq(0, 1, length = 9),
                     Var_4 = seq(0, 10, length = 9),
                     G = as.factor(1:9))

  I <- matrix(c(c("Var_1", "Var_2", "Var_3", "Var_4"),
                rep(1, 4),
                c("Var_1", "Var_2", "Var_3", "Var_4"),
                rep(0, 4),
                c(rep(5, 3), 10)), nrow = 4)

  colnames(I) <- c("cols", "w", "L", "Mins", "Maxs")

  Min <- 0.1
  Max <- 0.6

  expect_silent(make.zoom.plot(data = data, I = I,
                               Min = Min, Max = Max))
})
