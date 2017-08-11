#' @title Over sampling for time series classification
#' @description Oversample a time series sequence imbalance data.
#' @details This function balances a binary imbalance time series data. 
#' @references H. Cao, X.-L. Li, Y.-K. Woon and S.-K. Ng, 
#' @references "Integrated Oversampling for Imbalanced Time Series Classification"
#' @references IEEE Trans. on Knowledge and Data Engineering (TKDE), 
#' @references vol. 25(12), pp. 2809-2822, 2013
#' @param sample univariate sequence data samples
#' @param label labels corresponding to samples
#' @param target_class the label of the class which need to be oversampled
#' @param ratio targeted positive samples number to achieve/negative samples number, with the default value 1
#' @param Per Percentage of the mixing between SPO and ADASYN, with the default value 0.8
#' @param R An scalar ratio to tell in which level (towards the boundary) we shall push our syntactic data, 
#'          with the default value 1
#' @param k k-NN used in the ADASYN algorithm, with the default value 5
#' @param m m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15
#' @return sample: the time series sequences data oversampled
#' @return label: the label corresponding to each row of records
#' @importFrom stats na.omit 
#' @export OSTSC
#' @examples
#' # loading data
#' data(synthetic_control_TRAIN)   
#' # create feature and label data 
#' label <- synthetic_control_TRAIN[, c(1)]
#' sample <- synthetic_control_TRAIN[, -1] 
#' # oversample the class 1 to the same amount of class 0
#' MyData <- OSTSC(sample, label, target_class = 1)
#' # print the feature data after oversampling
#' MyData$sample
#' # print the label data after oversampling
#' MyData$label

OSTSC <- function(sample, label, target_class, ratio=1, Per=0.8, R=1, k=5, m=15) {
  # Oversample a time series sequence imbalance data.
  #
  # Args:
  #   sample:       univariate sequence data samples.
  #   label:        labels corresponding to samples.
  #   target_class: the label of the class which need to be oversampled.
  #   ratio:        targeted positive samples number to achieve/negative samples number, 
  #                 with the default value 1.
  #   Per:          Percentage of the mixing between SPO and ADASYN, with the default value 0.8.
  #   R:            An scalar ratio to tell in which level (towards the boundary) we shall push our 
  #                 syntactic data, with the default value 1.
  #   k:            k-NN used in the ADASYN algorithm, with the default value 5.
  #   m:            m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15.
  #
  # Returns:
  #   The oversampled dataset samples data_list$sample and labels data_list$label.
  
  # check if the numbers of records in label and sample matched
  if (is.null(dim(label))) {
    sizeLabel <- length(label)
  } else {
    sizeLabel <- dim(label)[1]
  }
  
  if (is.null(dim(sample))) {
    sizeSample <- length(sample)
  } else {
    sizeSample <- dim(sample)[1]
  }
  
  if (sizeLabel != sizeSample) {
    stop ("Number of time series sequences provided in sample do not match the number of classes provided in label.")
  }
  
  # check if the target_class input is only one element
  if (length(target_class) != 1) {
    stop ("The target_class is not in correct format, which must be a single value.")
  }
  
  fullData <- cbind(label, sample)
  fullData <- matrix(unlist(fullData, use.names = FALSE), ncol = ncol(fullData))
  
  # clean missing values and non-number values
  fullData <- matrix(suppressWarnings(as.numeric(fullData)), nrow = nrow(fullData))
  cleanData <- na.omit(fullData)
  
  # form positive (target class) data and negative data
  # The negative data is formed using a one-vs-rest manner.
  Positive <- cleanData[which(cleanData[, c(1)] == target_class), ]
  
  # check if Positive dataset is empty
  if (nrow(Positive) == 0) {
    stop ("The target_class does not exist in the input label.")
  }
  
  Negative <- cleanData[which(cleanData[, c(1)] != target_class), ]
  
  P <- Positive[, -1]
  N <- Negative[, -1]
  nTarget <- nrow(N)*ratio
  
  # oversampling
  myData <- INOS2013(P, N, nTarget, R, Per, k, m)
  
  # form new data
  data_target_class <- cbind(matrix(target_class, nTarget, 1), myData)
  
  data_new <- rbind(data_target_class, Negative)
  # data_new <- data_target_class
  data_x <- data_new[, -1]
  data_y <- data_new[, c(1)]
  data_list <- list("sample" = data_x, "label" = data_y)
  
  return(data_list)
}



