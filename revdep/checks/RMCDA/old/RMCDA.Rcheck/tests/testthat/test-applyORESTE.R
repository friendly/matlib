library(testthat)

test_that("apply.ORESTE works for provided example and returns the expected structure", {

  mat <- matrix(c(10, 2,
                                   20, 4,
                                   15, 5),
                                   nrow = 3, byrow = TRUE)
  weights <- c(0.7, 0.3)
  beneficial.vector <- c(1)   # 1st column "max", 2nd column "min"

  results <- apply.ORESTE(mat, weights, beneficial.vector, alpha = 0.4)

  expect_length(results, nrow(mat))

  expect_type(results, "double")
})
