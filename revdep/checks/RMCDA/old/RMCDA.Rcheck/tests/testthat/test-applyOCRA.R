library(testthat)

test_that("apply.OCRA works for provided example and returns the expected structure", {

  mat <- matrix(c(
       7.7, 256, 7.2, 7.3, 7.3,
       8.1, 250, 7.9, 7.8, 7.7,
       8.7, 352, 8.6, 7.9, 8.0,
       8.1, 262, 7.0, 8.1, 7.2,
       6.5, 271, 6.3, 6.4, 6.1,
       6.8, 228, 7.1, 7.2, 6.5
     ), nrow = 6, byrow = TRUE)

  weights <- c(0.239, 0.225, 0.197, 0.186, 0.153)
  beneficial.vector <- c(1, 3, 4, 5)

  results <- apply.OCRA(mat, weights, beneficial.vector)
  expect_length(results, nrow(mat))
  expect_type(results, "double")

})
