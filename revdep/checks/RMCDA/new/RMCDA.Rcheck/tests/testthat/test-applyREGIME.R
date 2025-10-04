library(testthat)

test_that("apply.REGIME works for provided example and returns the expected structure", {

  mat <- matrix(c(10, 5,
                  12, 4,
                  11, 6), nrow=3, byrow=TRUE)


  benef.vec <- c(1)  # means col1 is "max", col2 is "min"
  wts <- c(0.6, 0.4)

  results <- apply.REGIME(mat, benef.vec, wts, doPreOrder=FALSE)

  expect_length(results, nrow(mat)^2)

  expect_type(results, "character")
})
