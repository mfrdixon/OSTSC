#' @title Over Sampling for Time Series Classification
#' @description Oversample a univariate, multi-modal time series sequence of imbalanced classified data.
#' @details This function balances univariate imbalance time series data based on 
#' structure preserving oversampling.
#' @references H. Cao, X.-L. Li, Y.-K. Woon and S.-K. Ng, 
#'             "Integrated Oversampling for Imbalanced Time Series Classification" 
#'             IEEE Trans. on Knowledge and Data Engineering (TKDE), 
#'             vol. 25(12), pp. 2809-2822, 2013
#'             
#'             H. Cao, V. Y. F. Tan and J. Z. F. Pang, 
#'             "A Parsimonious Mixture of Gaussian Trees Model for Oversampling in Imbalanced and Multi-Modal Time-Series Classification" 
#'             IEEE Trans. on Neural Network and Learning System (TNNLS), 
#'             vol. 25(12), pp. 2226-2239, 2014
#'             
#'             H. Cao, X. L. Li, Y. K. Woon and S. K. Ng, 
#'             "SPO: Structure Preserving Oversampling for Imbalanced Time Series Classification" 
#'             Proc. IEEE Int. Conf. on Data Mining ICDM, 
#'             pp. 1008-1013, 2011
#' @param sample Univariate sequence data samples
#' @param label Labels corresponding to samples
#' @param class The number of the classes to be oversampled, starting 
#'              from the class with the fewest observations, with the default setting 
#'              to progress to as many classes as possible. 
#' @param ratio The oversampling ratio 
#'              number (>=1) (default = 1)
#' @param per Ratio of weighting between ESPO and ADASYN (default = 0.8) 
#' @param r A scalar ratio specifying which level (towards the boundary) we shall 
#'          push the synthetic data in ESPO (default = 1)
#' @param k Number of nearest neighbours in k-NN (for ADASYN) algorithm (default = 5)
#' @param m Seeds from the positive class in m-NN (for ADASYN) algorithm (default = 15) 
#' @param parallel Whether to execute in parallel mode (default = TRUE). 
#'                 (Recommended for datasets with over 30,000 records.)
#' @param progBar Whether to include progress bars (default = TRUE).
#'                For ESPO approach, the bar charactor is |--------|100\%. 
#'                For ADASYN approach, the bar charactor is |========|100\%.
#' @return sample: the time series sequences data oversampled
#' @return label: the label corresponding to each row of records
#' @importFrom stats na.omit 
#' @export OSTSC
#' @examples
#' # This is a simple example to show the usage of OSTSC. See the vignetter for a tutorial 
#' # demonstrating more complex examples.
#' # Example one
#' # loading data
#' data(Dataset_Synthetic_Control)
#' # get split feature and label data 
#' train.label <- Dataset_Synthetic_Control$train.y
#' train.sample <- Dataset_Synthetic_Control$train.x
#' # the first dimension of the feature set and labels must be the same
#' # the second dimension of the feature set is the sequence length
#' dim(train.sample)
#' dim(train.label)
#' # check the imbalance ratio of the data
#' table(train.label)
#' # oversample class 1 to the same number of observations as class 0
#' MyData <- OSTSC(train.sample, train.label, parallel = FALSE)
#' # store the feature data after oversampling
#' x <- MyData$sample
#' # store the label data after oversampling
#' y <- MyData$label
#' # check the imbalance of the data
#' table(y)
#' # Example two
#' # loading data
#' ecg <- Dataset_ECG()
#' # get split feature and label data 
#' train.label <- ecg$train.y
#' train.sample <- ecg$train.x
#' # the first dimension of the feature set and labels must be the same
#' # the second dimension of the feature set is the sequence length
#' dim(train.sample)
#' dim(train.label)
#' # check the imbalance ratio of the data
#' table(train.label)
#' # oversample class 3, 4, 5 to the same number of observations as class 1
#' MyData <- OSTSC(train.sample, train.label, parallel = FALSE)
#' # store the feature data after oversampling
#' x <- MyData$sample
#' # store the label data after oversampling
#' y <- MyData$label
#' # check the imbalance of the data
#' table(y)

OSTSC <- function(sample, label, class, ratio = 1, per = 0.8, r = 1, k = 5, 
                  m = 15, parallel = TRUE, progBar = TRUE) {
  # Oversample a time series sequence imbalance data.
  #
  # Args:
  #   sample:       Univariate sequence data samples.
  #   label:        Labels corresponding to samples.
  #   class:        The number of the classes to be oversampled, starting from the class with the fewest observations, with the default setting to progress to as many classes as possible 
  #   ratio:        The oversampling ratio 
  #              number (>=1) (default = 1)
  #   per:          Ratio of weighting between ESPO and ADASYN (default = 0.8) 
  #   r:            A scalar ratio specifying which level (towards the boundary) we shall push the synthetic data (in EPSO, default = 1)
  #   k:            k Number of nearest neighbours in k-NN (for ADASYN) algorithm (default = 5)
  #   m:            m Seeds from the positive class in m-NN (for ADASYN) algorithm (default = 15)
  #   parallel:     parallel Whether to execute in parallel mode (default = TRUE). 
 #                 (Recommended for datasets with over 30,000 records.)
  #   progBar:      Whether to include progress bars (default = TRUE).
  #
  # Returns:
  #   The oversampled dataset samples data_list$sample and labels data_list$label.
  
  # check if the input sample data had two dimension
  if (is.null(dim(sample)) || length(dim(sample)) != 2) {
    stop ("The input sample data must have two dimensions.")
  }
  
  # check if the numbers of records in label and sample matched
  if (is.null(dim(label))) {
    sizeLabel <- length(label)
  } else {
    sizeLabel <- dim(label)[1]
  }
  
    sizeSample <- dim(sample)[1]
  
  if (sizeLabel != sizeSample) {
    stop ("Number of time series sequences provided in sample do not match the 
          number of classes provided in label. Check dimensions.")
  }
  
  # check if the class input is in the numeric format
  if (!missing(class) && !is.numeric(class)) {
    stop ("The parameter class is not in correct format, which must be a numeric value.")
  }  
  
  # check if the ratio input is in the numeric format
  if (!is.numeric(ratio)) {
    stop ("The parameter ratio is not in correct format, which must be a numeric value.")
  }    
  
  # check if the Percentage input is in the numeric format
  if (!is.numeric(per)) {
    stop ("The parameter per is not in correct format, which must be a numeric value.")
  } 
  
  # check if the r input is in the numeric format
  if (!is.numeric(r)) {
    stop ("The parameter r is not in correct format, which must be a numeric value.")
  }
   
  # check if the k input is in the numeric format
  if (!is.numeric(k)) {
    stop ("The parameter k is not in correct format, which must be a numeric value.")
  }
   
  # check if the m input is in the numeric format
  if (!is.numeric(m)) {
    stop ("The parameter m is not in correct format, which must be a numeric value.")
  }
    
  # check if the class input is only one element
  if (!missing(class) && length(class) != 1) {
    stop ("The parameter class is not in correct format, which must be a single value.")
  }
  
  # check if the ratio input is only one element
  if (length(ratio) != 1) {
    stop ("The parameter ratio is not in correct format, which must be a single value.")
  }
  
  # check if the per input is only one element
  if (length(per) != 1) {
    stop ("The parameter per is not in correct format, which must be a single value.")
  }
    
  # check if the R input is only one element
  if (length(r) != 1) {
    stop ("The parameter r is not in correct format, which must be a single value.")
  }
    
  # check if the k input is only one element
  if (length(k) != 1) {
    stop ("The parameter k is not in correct format, which must be a single value.")
  }
    
  # check if the m input is only one element
  if (length(m) != 1) {
    stop ("The parameter m is not in correct format, which must be a single value.")
  }
  
  # check if the ratio input is in range (0,1]
  if (ratio > 1 || ratio <= 0) {
    stop ("The parameter ratio is not in correct range, which must be betwwen 
          0 to 1, including 1.")
  }
    
  # check if the Percentage input is in range [0,1]
  if (per > 1 || per < 0) {
    stop ("The parameter per is not in correct range, which must be betwwen 
          0 to 1, including 0 and 1.")
  }
    
  # check if the R input is in range [1,+oo)
  if (r < 1) {
    stop ("The parameter r is not in correct range, which must be larger or 
          equal to 1.")
  }
    
  # check if the k input is in range (0,+oo)
  if (k <= 0) {
    stop ("The parameter k is not in correct range, which must be larger than 0.")
  }
    
  # check if the m input is in range (0,+oo)
  if (m <= 0) {
    stop ("The parameter m is not in correct range, which must be larger than 0.")
  }
    
  # check if the k input is an integer
  if (k %% 1 != 0) {
    stop ("The parameter k is not in correct format, which must be an integer.")
  }
    
  # check if the m input is an integer
  if (m%%1 != 0) {
    stop ("The parameter m is not in correct format, which must be an integer.")
  }
    
  # check if the parallel input is a boolean value
  if (!(identical(parallel, FALSE) || identical(parallel, TRUE))) {
    stop ("The parameter parallel is not in correct format, which must be a 
          boolean value.")
  }
  
  # check if the progBar input is a boolean value
  if (!(identical(progBar, FALSE) || identical(progBar, TRUE))) {
    stop ("The parameter progBar is not in correct format, which must be a 
          boolean value.")
  }
  
  # combine labels and features
  fullData <- cbind(label, sample)
  fullData <- matrix(unlist(fullData, use.names = FALSE), 
                     ncol = ncol(fullData))
  
  # clean missing values and non-number values by removing their belonging rows
  fullData <- matrix(suppressWarnings(as.numeric(fullData)), 
                     nrow = nrow(fullData))
  cleanData <- na.omit(fullData)
  
  # determine how many classes need to be oversampled
  Lab <- cleanData[, c(1)]
  claTab <- as.data.frame(table(Lab))  # count frequency of classes
  claTab <- claTab[order(claTab$Freq), ]  # order in ascending
  
  sumFreq <- sum(claTab$Freq)
  
  count <- 0
  for (i in 1:dim(claTab)[1]) {
    if (sumFreq - claTab$Freq[i] > claTab$Freq[i]) {
      count <- count + 1
    }
  }
  
  if (count == 0) {
    stop ("The input dataset is already balanced. No oversampling is necessary.")
  }
  
  if (missing(class)) {
    class <- count
  } 
  
  if (count < class) {
    warning ("Insufficient observations of the minority class. The class number that needs to be oversampled 
             is set to ", count)
  }
  
  myData <- list()
  for (i in 1:class) {
    targetClass <- as.numeric(as.vector(claTab$Lab[i]))
    newData <- ReguCovar(cleanData, targetClass, ratio, r, per, k, m, 
                         parallel, progBar)
    myData <- rbind(myData, newData)
  }
  
  nData <- list()
  for (i in (class + 1):dim(claTab)[1]) {
    targetClass <- as.numeric(as.vector(claTab$Lab[i]))
    nega <- cleanData[which(cleanData[, c(1)] == targetClass), ]
    nData <- rbind(nData, nega)
  }
  
  # form data
  dataNew <- rbind(myData, nData)
  dataNew <- matrix(unlist(dataNew), ncol=ncol(dataNew))
  
  dataX <- dataNew[, -1]
  dataY <- dataNew[, c(1)]
  dataList <- list("sample" = dataX, "label" = dataY)
  
  return(dataList)
}



