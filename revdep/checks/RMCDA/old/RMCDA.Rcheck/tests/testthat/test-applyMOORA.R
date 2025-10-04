library(testthat)

test_that("apply.MOORA works for provided example and returns the expected structure", {

  mat <- matrix(c(60, 6.35, 6.8, 10, 2.5, 4.5, 3,
   0.4, 0.15, 0.1, 0.2, 0.1, 0.08, 0.1,
   2540, 1016, 1727.2, 1000, 560, 1016, 177,
   500, 3000, 1500, 2000, 500, 350, 1000,
   990, 1041, 1676, 965, 915, 508, 920), nrow=7)
   colnames(mat)<-c("Load capacity", "Repeatability", "Maximum tip speed",
   "Memory capacity", "Manipulator reach")
  rownames(mat)<-paste0("A", 1:7)
  weights <- c(0.1574, 0.1825, 0.2385, 0.2172, 0.2043)
  beneficial.vector <- c(1, 3, 4, 5)
  results <- apply.MOORA(mat, weights, beneficial.vector)

  expect_length(results, nrow(mat))
  expect_type(results, "double")

})
