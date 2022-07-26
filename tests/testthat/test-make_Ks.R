test_that("multiplication works", {


  m <- 300
  vars <- list(
    list("Var_1", 1, "Var 1", 0, 5),
    list("Var_2", 1, "Var 2", 0, 5),
    list("Var_3", 1, "Var 3", 0, 5),
    list("Var_4", 1, "Var 4", 0, 5)
  )

  df <- data.frame("Var_1" = seq(10, 5, length = 10),
                   "Var_2" = seq(30, 50, length = 10),
                   "Var_3" = seq(0, 1, length = 10),
                   "Var_4" = seq(20, 30, length = 10))

  S <- "glpk"
  ToC <- 2

  test <- make.Ks(M = m, vars = vars, D = df, S = S, ToC = ToC)

  expect_length(test, 7)
  expect_true(is.list(test))
  expect_true(is.character(test[[2]]))
  expect_true(is.data.frame(test[[3]]))

  expect_equal(sum(data.frame("Row number" = c(1, 3, 5, 7, 9),
                          "Match" = c(2, 4, 6, 8, 10)) ==
               test[[4]]), 10)

})
