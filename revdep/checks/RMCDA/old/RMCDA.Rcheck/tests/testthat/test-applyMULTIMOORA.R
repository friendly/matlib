library(testthat)

test_that("apply.MULTIMOORA works for provided example and returns the expected structure", {

  mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
                                  420, 91, 1365, 1120, 875, 1190, 200,
                                  74.2, 70, 189, 210, 112, 217, 112,
                                  2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53), nrow = 4, byrow = TRUE)
  beneficial.vector <- c(1, 3) # Columns 1 and 3 are beneficial
  result <- apply.MULTIMOORA(mat, beneficial.vector)

  expect_type(result[[1]], "double")
  expect_type(result[[2]], "double")
  expect_type(result[[3]], "double")


})
