library(testthat)

test_that("apply.RAFSI works for provided example and returns the expected structure", {

  mat <- matrix(c(3, 2, 5,
                  4, 3, 2,
                  1, 6, 4),
                  nrow = 3, byrow = TRUE)
  weights <- c(0.3, 0.5, 0.2)
  beneficial.vector <- c(1, 2)
  results <- apply.RAFSI(mat, weights, beneficial.vector,   n_i = 1, n_k = 6)

  expect_length(results, nrow(mat))

  expect_type(results, "double")
})
