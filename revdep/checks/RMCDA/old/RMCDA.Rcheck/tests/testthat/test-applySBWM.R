library(testthat)

test_that("apply.SBWM works for provided example and returns the expected structure", {

  data <- read.csv(system.file("extdata", "stratified_BWM_case_study_I_example.csv", package = "RMCDA"), header = FALSE)
  mat.lst <- read.csv.SBWM.matrices(data)
  comparison.mat <- mat.lst[[1]]
  others.to.worst <- mat.lst[[2]]
  others.to.best <- mat.lst[[3]]
  state.worst.lst <- mat.lst[[4]]
  state.best.lst <- mat.lst[[5]]
  likelihood.vector <- mat.lst[[6]]
  results <- apply.SBWM(comparison.mat, others.to.worst, others.to.best, state.worst.lst, state.best.lst, likelihood.vector)


  expect_length(results[,1], ncol(comparison.mat))

  expect_type(results[,1], "double")
})
