context("input other parameters")

test_that("input parameters class, rario, Per, R, k, m are numetic values", {
  # create sample and label data
  data("synthetic_control")
  train_label <- synthetic_control$train_y
  train_sample <- synthetic_control$train_x
  # oversampling
  expect_error( OSTSC(train_sample, train_label, class = "1"), "The parameter class is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, ratio = "1"), "The parameter ratio is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, Per = "1"), "The parameter Per is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, R = "1"), "The parameter R is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, k = "1"), "The parameter k is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, m = "1"), "The parameter m is not in correct format" )
})

test_that("input parameters rario, Per, R, k, m locate in correct range", {
  # create sample and label data
  data("synthetic_control")
  train_label <- synthetic_control$train_y
  train_sample <- synthetic_control$train_x
  # oversampling
  expect_error( OSTSC(train_sample, train_label, ratio = 0), "The parameter ratio is not in correct range" )
  expect_error( OSTSC(train_sample, train_label, Per = 2), "The parameter Per is not in correct range" )
  expect_error( OSTSC(train_sample, train_label, R = 0), "The parameter R is not in correct range" )
  expect_error( OSTSC(train_sample, train_label, k = 0), "The parameter k is not in correct range" )
  expect_error( OSTSC(train_sample, train_label, k = 0.6), "The parameter k is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, m = 0), "The parameter m is not in correct range" )
  expect_error( OSTSC(train_sample, train_label, m = 1.6), "The parameter m is not in correct format" )
})

test_that("input parameters class, rario, Per, R, k, m is a single value", {
  # create sample and label data
  data("synthetic_control")
  train_label <- synthetic_control$train_y
  train_sample <- synthetic_control$train_x
  # oversampling
  expect_error( OSTSC(train_sample, train_label, class = c(1, 1)), "The parameter class is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, ratio = c(1, 1)), "The parameter ratio is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, Per = c(1, 1)), "The parameter Per is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, R = c(1, 1)), "The parameter R is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, k = c(1, 1)), "The parameter k is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, m = c(1, 1)), "The parameter m is not in correct format" )
})

test_that("input parameters parallel, progBar is boolean value", {
  # create sample and label data
  data("synthetic_control")
  train_label <- synthetic_control$train_y
  train_sample <- synthetic_control$train_x
  # oversampling
  expect_error( OSTSC(train_sample, train_label, parallel = 1), "The parameter parallel is not in correct format" )
  expect_error( OSTSC(train_sample, train_label, progBar = 1), "The parameter progBar is not in correct format" )
})