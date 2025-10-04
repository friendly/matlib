library(testthat)

test_that("apply.MAIRCA works for provided example and returns the expected structure", {

  mat <- matrix(c(70, 245, 16.4, 19,
                                   52, 246, 7.3, 22,
                                   53, 295, 10.3, 25,
                                   63, 256, 12.0, 8,
                                   64, 233, 5.3, 17),
                                   nrow = 5, byrow = TRUE)
  weights <- c(0.04744, 0.02464, 0.51357, 0.41435)
  types <- c(1, 1, 1, 1)
  result <- apply.MAIRCA(mat, weights, types)

  expect_length(result, nrow(mat))
  expect_type(result, "double")

})
