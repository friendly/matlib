library(testthat)

test_that("apply.BWM works for the example provided and returns the expected structure", {

  criteria.lst <- c("C1", "C2", "C3")
  worst.criteria <- "C1"
  best.criteria <- "C3"
  best.criteria.preference <- c(8, 2, 1)
  worst.criteria.preference <- c(1, 5, 8)
  results <- apply.BWM(criteria.lst, worst.criteria, best.criteria, best.criteria.preference, worst.criteria.preference)
  #'

  expect_length(results, 4)

  expect_type(results, "double")

})
