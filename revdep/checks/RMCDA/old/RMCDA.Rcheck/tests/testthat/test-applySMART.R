library(testthat)

test_that("apply.SMART works for provided example and returns the expected structure", {

  data_mat <- matrix(c(10, 20, 15,  7, 30,  5,  8, 25),nrow = 2, byrow = TRUE)

  benef_vec <- c(1, 2, 3)

  grades <- c(2, 2, 1, 3)
  lower  <- c(0, 0, 0,  0)
  upper  <- c(40, 40, 40, 40)


  result <- apply.SMART(dataset = data_mat,
                                           grades = grades,
                                           lower  = lower,
                                           upper  = upper,
                                           beneficial.vector = benef_vec)

  expect_length(result[,1], nrow(data_mat))

  expect_type(result[,1], "double")
  expect_type(result[,2], "double")

})
