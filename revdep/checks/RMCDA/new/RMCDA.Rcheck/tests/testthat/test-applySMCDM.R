library(testthat)

test_that("apply.SMCDM works for provided example and returns the expected structure", {

  data <- read.csv(system.file("extdata", "SMCDM_input.csv", package = "RMCDA"), header=FALSE)
  mat.lst <- read.csv.SMCDM.matrices(data)
  comparison.mat <- mat.lst[[1]]
  state.criteria.probs <- mat.lst[[2]]
  likelihood.vector <- mat.lst[[3]]
  results <- apply.SMCDM(comparison.mat, state.criteria.probs, likelihood.vector)


  expect_length(results[,1], nrow(comparison.mat))

  expect_type(results[,1], "double")
})
