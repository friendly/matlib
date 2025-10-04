library(testthat)

test_that("apply.COPELAND works for provided example and returns the expected structure", {

  mat <- matrix(c(80, 60, 90,
                   75, 85, 95,
                   70, 65, 85,
                   60, 75, 80),
                 nrow = 4, byrow = TRUE)
  colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
  beneficial.vector <- c(1, 2, 3)
  result <- apply.COPELAND(mat, beneficial.vector)

  expect_length(result, 4)

  expect_type(result, "double")


})
