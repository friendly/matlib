library(testthat)
library(lpSolve)
test_that("apply.OPA works for provided example and returns the expected structure", {

  expert.x.alt <- matrix(c(1, 3, 2, 2, 1, 3), nrow = 3)
  colnames(expert.x.alt) <- c("c", "q")
  rownames(expert.x.alt) <- c("alt1", "alt2", "alt3")

  expert.y.alt <- matrix(c(1, 2, 3, 3, 1, 2), nrow = 3)
  colnames(expert.y.alt) <- c("c", "q")
  rownames(expert.y.alt) <- c("alt1", "alt2", "alt3")

  expert.opinion.lst <- list(expert.x.alt, expert.y.alt)
  expert.rank <- c(1, 2)

  criterion.x.rank <- c(1, 2)
  criterion.y.rank <- c(2, 1)
  criterion.rank.lst <- list(criterion.x.rank, criterion.y.rank)

  results <- apply.OPA(expert.opinion.lst, expert.rank, criterion.rank.lst)

  expect_length(results[[1]][,1], 3)
  expect_length(results[[2]][,1], 3)

  expect_length(results[[1]][1,], 2)
  expect_length(results[[2]][1,], 2)

  expect_type(results[[1]][1], "double")
  expect_type(results[[2]][1], "double")
})
