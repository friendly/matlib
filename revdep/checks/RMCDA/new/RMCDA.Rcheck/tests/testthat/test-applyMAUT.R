library(testthat)

test_that("apply.MAUT works for provided example and returns the expected structure", {

  mat <- matrix(c(75.5, 95, 770,
                                   187, 179, 239,
                                   237, 420, 91), nrow = 3, byrow = TRUE)

  weights <- c(0.3, 0.5, 0.2)

  beneficial.vector <- c(1, 3)
  utility.functions <- c("exp", "log", "quad")

  step.size <- 1

  result <- apply.MAUT(mat, weights, beneficial.vector, utility.functions, step.size)
  expect_type(result, "double")

  expect_length(result, nrow(mat)*2)

})
