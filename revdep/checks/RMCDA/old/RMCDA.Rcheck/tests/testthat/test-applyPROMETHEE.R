library(testthat)

test_that("apply.PROMETHEE works for provided example and returns the expected structure", {

  A <- matrix(c(250, 200, 300, 275, 16, 16, 32, 32, 12, 8, 16, 8, 5, 3, 4, 2), nrow=4)
  rownames(A)<-c("Mobile 1", "Mobile 2", "Mobile 3", "Mobile 4")
  colnames(A)<-c("Price", "Memory", "Camera", "Looks")
  weights <- c(0.35, 0.25, 0.25, 0.15)
  results <- apply.PROMETHEE(A, weights)

  expect_length(results, nrow(A))

  expect_type(results, "list")
})
