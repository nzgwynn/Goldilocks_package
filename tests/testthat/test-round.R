test_that("rounding for the zoom plot actually works", {

  test_1 <- Round(Min = .0245, Max = .1534, N = 20)
  expect_equal(test_1, c(1,4))

  test_2 <- Round(Min = .0333, Max = .1398, N = 18)
  expect_equal(test_2, c(1,4))

  test_3 <- Round(Min = .0209, Max = .0982, N = 15)
  expect_equal(test_3, c(1,3))

  test_4 <- Round(Min = .902, Max = .982, N = 12)
  expect_equal(test_4, c(10,12))

})
