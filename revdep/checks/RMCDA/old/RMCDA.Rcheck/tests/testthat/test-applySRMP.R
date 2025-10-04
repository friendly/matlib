library(testthat)

test_that("apply.SRMP works for provided example and returns the expected structure", {

  evaluations.mat <- matrix(c(41, 46, 43, -2, -4, -5.5, 4, 2, 3), nrow=3)
  colnames(evaluations.mat) <- c("S", "L", "J")
  rownames(evaluations.mat) <- c("x", "y", "z")
  reference.profiles <- matrix(c(42, 45, -5, -3, 2, 4), nrow=2)
  colnames(reference.profiles) <- c("S", "L", "J")
  rownames(reference.profiles) <- c("p1", "p2")
  weights <- c(1/3, 1/3, 1/3)
  results <- apply.SRMP(evaluations.mat, reference.profiles, weights)

  expect_length(results, nrow(evaluations.mat))

  expect_type(results, "double")
})
