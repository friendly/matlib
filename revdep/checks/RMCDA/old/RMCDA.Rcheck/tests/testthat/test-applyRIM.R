library(testthat)

test_that("apply.RIM works for provided example and returns the expected structure", {

  mat <- matrix(
      c(30,40,25,27,45,0,
        9,0,0,15,2,1,
        3,5,2,3,3,1,
        3,2,3,3,3,2,
        2,2,1,4,1,2),
      nrow = 5, ncol = 6, byrow = TRUE
    )


  weights <- c(0.2262,0.2143,0.1786,0.1429,0.119,0.119)


    AB <- matrix(
      c(23,60,0,15,0,10,
        1,3,1,3,1,5),
      nrow = 2, ncol = 6, byrow = TRUE
    )


    CD <- matrix(
      c(30,35,10,15,0,0,
        3,3,3,3,4,5),
      nrow = 2, ncol = 6, byrow = TRUE
    )


  results <- apply.RIM(mat, weights, AB, CD)

  expect_length(results$Alternatives, nrow(mat))

  expect_type(results$Ranking, "integer")
  expect_type(results$R, "double")
})
