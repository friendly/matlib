library(testthat)

test_that("apply.BORDA works for a consistent matrix and returns the expected structure", {

  mat <- matrix(c(
       5, 9, 2,
       7, 3, 8,
       6, 5, 4,
       4, 7, 9
     ), nrow = 4, byrow = TRUE)

     # Suppose columns 1 and 3 are beneficial
     beneficial.vector <- c(1, 3)
     results <- apply.BORDA(mat, beneficial.vector)
     #' borda_scores
     expect_length(results, 4)

     expect_type(results, "double")

})
