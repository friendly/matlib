library(testthat)

test_that("apply.MARCOS works for provided example and returns the expected structure", {

  mat <- matrix(c(660, 1000, 1600, 18, 1200,
                                   800, 1000, 1600, 24, 900,
                                   980, 1000, 2500, 24, 900,
                                   920, 1500, 1600, 24, 900,
                                   1380, 1500, 1500, 24, 1150,
                                   1230, 1000, 1600, 24, 1150,
                                   680, 1500, 1600, 18, 1100,
                                   960, 2000, 1600, 12, 1150), nrow = 8, byrow = TRUE)
  weights <- c(0.1061, 0.3476, 0.3330, 0.1185, 0.0949)
  beneficial.vector <- c(2, 3, 4, 5)  # Columns 2, 3, 4, and 5 are beneficial
  results <- apply.MARCOS(mat, weights, beneficial.vector)
  expect_length(results, nrow(mat))
  expect_type(results, "double")

})
