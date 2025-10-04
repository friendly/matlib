library(testthat)

test_that("apply.SPOTIS works for provided example and returns the expected structure", {

  matrix <- matrix(c(10.5, -3.1, 1.7,
                     -4.7, 0, 3.4,
                     8.1, 0.3, 1.3,
                     3.2, 7.3, -5.3), nrow = 4, byrow = TRUE)


  bounds <- matrix(c(-5, 12,-6, 10,-8, 5), nrow = 3, byrow = TRUE)


  weights <- c(0.2, 0.3, 0.5)


  types <- c(1, -1, 1)


  results <- apply.SPOTIS(matrix, weights, types, bounds)

  expect_length(results, nrow(matrix))

  expect_type(results, "double")
})
