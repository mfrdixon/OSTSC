context("input other parameters")

test_that("input parameters target_class, rario, Per, R, k, m are numetic values", {
  # create sample and label data
  data("dataset_synthetic_control")
  train_label <- dataset_synthetic_control$train_y
  train_sample <- dataset_synthetic_control$train_x
  # oversampling
  expect_error( OSTSC(train_sample, train_label, target_class = "1"), "The parameter target_class is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, ratio = "1"), "The parameter ratio is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, Per = "1"), "The parameter Per is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, R = "1"), "The parameter R is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, k = "1"), "The parameter k is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, m = "1"), "The parameter m is not in correct format" )
})

test_that("input parameters target_class, rario, Per, R, k, m locate in correct range", {
  # create sample and label data
  data("dataset_synthetic_control")
  train_label <- dataset_synthetic_control$train_y
  train_sample <- dataset_synthetic_control$train_x
  # oversampling
  expect_error( OSTSC(train_sample, train_label, target_class = 2), "The target_class does not exist in the input label" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, ratio = 0), "The parameter ratio is not in correct range" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, Per = 2), "The parameter Per is not in correct range" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, R = 0), "The parameter R is not in correct range" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, k = 0), "The parameter k is not in correct range" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, k = 0.6), "The parameter k is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, m = 0), "The parameter m is not in correct range" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, m = 1.6), "The parameter m is not in correct format" )
})

test_that("input parameters target_class, rario, Per, R, k, m is a single value", {
  # create sample and label data
  data("dataset_synthetic_control")
  train_label <- dataset_synthetic_control$train_y
  train_sample <- dataset_synthetic_control$train_x
  # oversampling
  expect_error( OSTSC(train_sample, train_label, target_class = c(1, 1)), "The parameter target_class is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, ratio = c(1, 1)), "The parameter ratio is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, Per = c(1, 1)), "The parameter Per is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, R = c(1, 1)), "The parameter R is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, k = c(1, 1)), "The parameter k is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, m = c(1, 1)), "The parameter m is not in correct format" )
})

test_that("input parameters parallel, progBar is boolean value", {
  # create sample and label data
  data("dataset_synthetic_control")
  train_label <- dataset_synthetic_control$train_y
  train_sample <- dataset_synthetic_control$train_x
  # oversampling
  expect_error( OSTSC(train_sample, train_label, target_class = 1, parallel = 1), "The parameter parallel is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, target_class = 1, progBar = 1), "The parameter progBar is not in correct format" )
})