library(xts)
context("input data")

test_that("input data could be in xts format", {
  # create xts data
  data("synthetic_control")
  train_label <- synthetic_control$train_y
  train_sample <- synthetic_control$train_x
  train <- cbind(train_label, train_sample)
  df_xts <- as.xts(train,order.by=Sys.time()+1:dim(train)[1])
  df_label <- df_xts[, c(1)]
  df_sample <- df_xts[, -1]
  # oversampling
  MyData <- OSTSC(df_sample, df_label)
  label <- MyData$label
  expect_equal( length(label[which(label==0)]), length(label[which(label==1)]) )
})

test_that("input data could be in array format", {
  # create array
  data("synthetic_control")
  train_label <- synthetic_control$train_y
  train_sample <- synthetic_control$train_x
  df_label <- as.array(train_label)
  df_sample <- as.array(train_sample)
  # oversampling
  MyData <- OSTSC(df_sample, df_label)
  label <- MyData$label
  expect_equal( length(label[which(label==0)]), length(label[which(label==1)]) )
})

test_that("input data could be in data frame format", {
  # create data frame
  data("synthetic_control")
  train_label <- synthetic_control$train_y
  train_sample <- synthetic_control$train_x
  df_label <- as.data.frame(train_label)
  df_sample <- as.data.frame(train_sample)
  # oversampling
  MyData <- OSTSC(df_sample, df_label)
  label <- MyData$label
  expect_equal( length(label[which(label==0)]), length(label[which(label==1)]) )
})

test_that("input sample and label data have same first dimension", {
  # create samples and labels with different records
  data("synthetic_control")
  train_label <- synthetic_control$train_y
  train_sample <- synthetic_control$train_x
  df_label <- train_label
  df_sample <- train_sample[-1, ]
  # oversampling
  expect_error( OSTSC(df_sample, df_label), "Number of time series sequences provided" )
})

test_that("OSTSC handles datasets containing NA, NaN, or accidental strings", {
  # create data containing NA, NaN, and accidental strings
  data("synthetic_control")
  train_label <- synthetic_control$train_y
  train_sample <- synthetic_control$train_x
  train_sample[1, 3] <- NaN
  train_sample[4, 1] <- NA 
  train_sample[5, 5] <- 'a'
  # oversampling
  MyData <- OSTSC(train_sample, train_label)
  label <- MyData$label
  expect_equal( length(label[which(label==0)]), length(label[which(label==1)]) )
})