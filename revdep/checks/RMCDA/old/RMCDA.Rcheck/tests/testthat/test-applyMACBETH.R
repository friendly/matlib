library(testthat)

test_that("apply.MACBETH works for provided example and returns the expected structure", {

  mat <- matrix(c(10, 5,
                   12, 4,
                   11, 6), nrow=3, byrow=TRUE)

  # Suppose first column is beneficial, second is non-beneficial
  benef.vec <- c(1)
  wts <- c(0.6, 0.4)

  # Get MACBETH scores
  results <- apply.MACBETH(mat, benef.vec, wts)

  expect_length(results, 3)
  expect_type(results, "double")

})
