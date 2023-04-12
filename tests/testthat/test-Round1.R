test_that("Rounding for the coloring actually works", {

  test_1 <- Round1(Min = .2045, Max = .5234, DC = .43 , K = 20)
  expect_equal(test_1, 7)

  test_2 <- Round1(Min = .0333, Max = .1398, DC = .098 ,K = 18)
  expect_equal(test_2, 1)

  test_3 <- Round1(Min = .0209, Max = .0982, DC = .5, K = 15)
  expect_equal(test_3, 2)

  test_4 <- Round1(Min = .902, Max = .982, DC = .97,K = 12)
  expect_equal(test_4, 12)

})
