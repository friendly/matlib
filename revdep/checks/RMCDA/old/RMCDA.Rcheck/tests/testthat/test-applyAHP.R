library(testthat)

test_that("apply.AHP works for a consistent matrix and returns the expected structure", {

  data <- read.csv(system.file("extdata", "AHP_input_file.csv", package = "RMCDA"), header=FALSE)

  mat.lst <- read.csv.AHP.matrices(data)


  mat.lst[[1]]->A
  mat.lst[[2]]->comparing.competitors
  result<- apply.AHP(A, comparing.competitors)

  # 'result' should be a list of length 4:
  # 1. criteria.weight (which is itself a list: list(CI/RI, W))
  # 2. criteria.alternatives.mat (data frame)
  # 3. weighted.scores.mat (matrix or data frame)
  # 4. alternative.score (numeric vector)

  expect_length(result, 4)

  #I) criteria.weight
  expect_type(result[[1]], "list")           # Must be a list
  expect_length(result[[1]], 2)             # Should have 2 elements: (CI/RI, W)
  expect_type(result[[1]][[1]], "double")   # CI/RI ratio is numeric
  expect_type(result[[1]][[2]], "double")   # Weight vector W is numeric

  #II) criteria.alternatives.mat
  criteria_alternatives <- result[[2]]
  expect_s3_class(criteria_alternatives, "data.frame")

  expect_equal(dim(criteria_alternatives), c(4, 4))

  #III) weighted.scores.mat
  weighted_scores_mat <- result[[3]]
  # Should match the same dimension as criteria.alternatives.mat
  expect_equal(dim(weighted_scores_mat), c(4, 4))

  #IV) alternative.score
  alt_score <- result[[4]]
  # Should be a numeric vector with length = # of alternatives = 4
  expect_type(alt_score, "double")
  expect_length(alt_score, 4)


})

