#' The high frequency trading data
#'
#' The data is a random selection from a high frequency trading data. The feature is from instantaneous 
#' liquidity imbalance using the best bid to ask ratio, up-tick as class 1, down-tick as class -1, and
#' normal status as class 0. The time series sequences length is set to 10. In this package, the class 1
#' and class -1 observations are random selected till 600, while the class 0 is 28800. While the whole 
#' observations are ordered in the time order, the dataset haven't split training and setting data. The 
#' users can split it by any ratio they like.
#'
#' @format A time series data with sequence length 10 and observations number 30000. 
#' \describe{
#'   The train_y and test_y contain the class label (1, -1 or 0)
#'   The train_x and test_x contain time series sequences (in numeric)
#'   Each sequence occurs in one line.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name HFT
#' @usage data(HFT)
#' @references Matthew Dixon.(2017) Sequence Classification of the Limit Order Book using Recurrent Neural Networks. 
#'             arXiv:1707.05642.

dataset_HFT <- function() {
  # load raw data
  toy_HFT <- rio::import("https://github.com/lweicdsor/GSoC2017/raw/master/toy_HFT/toy_HFT.rda")
  # set time sequences length as 10
  tranData <- matrix(0, dim(toy_HFT)[1]-9, 10)
  for (i in 1:(dim(toy_HFT)[1]-9)){
    tranData[i, ] <- toy_HFT[c(i:(i+9)), 1]
  }
  fullData <- cbind(matrix(1:(dim(toy_HFT)[1]-9), dim(toy_HFT)[1]-9, 1), matrix(toy_HFT[10:dim(toy_HFT)[1], 2], ncol = 1), tranData)
  # random select 600, 600, 28800 observations
  minus1 <- fullData[which(fullData[, c(2)]==-1), ]
  plus1 <- fullData[which(fullData[, c(2)]==1), ]
  zero0 <- fullData[which(fullData[, c(2)]==0), ]
  m1_new <- minus1[sample(nrow(minus1), 600), ]
  p1_new <- plus1[sample(nrow(plus1), 600), ]
  z0_new <- zero0[sample(nrow(zero0), 28800), ]
  newData <- rbind(m1_new, p1_new, z0_new)
  newData <- newData[order(newData[ ,1]), ]
  # split label and sample data
  y <- newData[, 2]
  x <- newData[, c(-1, -2)]
  x <- matrix(unlist(x), ncol = ncol(x))
  # form data
  HFT <- list("x" = x, "y" = y)
  # save data
  devtools::use_data(HFT)
}
