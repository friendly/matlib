library(testthat)
library(pracma)

test_that("apply.CILOS works for provided example and returns the expected structure", {

  mat <- matrix(
    c(75.5, 95, 770, 187, 179, 239, 237,
      420, 91, 1365, 1120, 875, 1190, 200,
      74.2, 70, 189, 210, 112, 217, 112,
      2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
      21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
      0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
      0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06),
    nrow = 7, byrow = TRUE
  )
  beneficial.vector <- c(1, 2, 3, 6, 7)
  result <- apply.CILOS(mat, beneficial.vector)

  expect_type(result, "double")
  expect_length(result, nrow(mat))

})


#'
