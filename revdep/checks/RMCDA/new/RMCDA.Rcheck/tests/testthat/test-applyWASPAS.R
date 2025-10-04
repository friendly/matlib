library(testthat)
library(matrixStats)
test_that("apply.WASPAS works for provided example and returns the expected structure", {

  mat <- matrix(c(0.04, 0.11, 0.05, 0.02, 0.08, 0.05, 0.03, 0.1, 0.03,
                  1.137, 0.854, 1.07, 0.524, 0.596, 0.722, 0.521, 0.418, 0.62,
                  960, 1920, 3200, 1280, 2400, 1920, 1600, 1440, 2560), nrow=9)
  colnames(mat)<-c("Dimensional Deviation (DD)", "Surface Roughness (SR)",
                  "Material Removal Rate (MRR)")

  rownames(mat)<-paste0("A", 1:9)
  beneficial.vector <- c(3)
  weights <- c(0.1047, 0.2583, 0.6369)
  results <- apply.WASPAS(mat, weights, beneficial.vector, 0.5)
  #'
  expect_type(results, "double")
  expect_length(results, nrow(mat))

})
