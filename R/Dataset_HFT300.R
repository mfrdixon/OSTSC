#' @title The high frequency trading data.
#'
#' @description This dataset is a random subset of a high frequency trading dataset used to assess the performace of RNNs for prediction (Dixon, 2017). 
#' @details The feature represents the instantaneous liquidity imbalance using the best bid to ask ratio. The labels represent the next-event mid-price movement - Y=1 is an up-tick, Y=-1 is a down-tick and Y=0 represents no-movement. The time series sequences length is set 
#' to 10. In this package, the class 1 and -1 observations are random selected to yield 12 non-zero observations, while class 0 has 288 observations. Observations are ordered chronologically.
#'
#' @format A dataset with 300 observations of sequence length = 10, with a single sequence per row. 
#' \describe{
#'   The y data is labeled as {-1,0,1}.
#'   
#'   The x data constructs time series sequences (numeric).
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name Dataset_HFT300
#' @usage data(Dataset_HFT300)
#' @references Matthew Dixon.(2017) Sequence Classification of the Limit Order Book using Recurrent Neural Networks. 
#'             arXiv:1707.05642.
NULL
