test_that("Calculate min value of a vector",{
  expect_equal(energetic.min(c(10, 20, 30)), 10)
  expect_equal(energetic.min(c(15, 21, 35)), 15)
})

