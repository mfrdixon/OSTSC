#' @title The high frequency trading data.
#'
#' @description This dataset is a random subset of a high frequency trading dataset used to assess the performace of RNNs for prediction (Dixon, 2017). 
#' @details The feature represents the instantaneous liquidity imbalance using the best bid to ask ratio. The labels represent the next-event mid-price movement - Y=1 is an up-tick, Y=-1 is a down-tick and Y=0 represents no-movement. The time series sequences length is set 
#' to 10. In this package, the class 1 and -1 observations are random selected to yield 1200 non-zero observations, while class 0 has 28800 observations. Observations are ordered chronologically.
#'
#' @format A dataset with 30000 observations of sequence length = 10, with a single sequence per row. 
#' \describe{
#'   The y data is labeled as {-1,0,1}.
#'   
#'   The x data constructs time series sequences (numeric).
#' }
#' 
#' @references Matthew Dixon.(2017) Sequence Classification of the Limit Order Book using Recurrent Neural Networks. 
#'             arXiv:1707.05642.
#'             
#' @return hft: the dataset HFT
#' @export Dataset_HFT

Dataset_HFT <- function(){
  hft <- local(get(load(url('https://github.com/lweicdsor/GSoC2017/raw/master/toy_HFT/Dataset_HFT.rdata'))))
  return (hft)
}