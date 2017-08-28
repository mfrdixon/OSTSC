#' @title Over Sampling for Time Series Classification
#' @description Oversample a time series sequence imbalance data.
#' @details This function balances a binary imbalance time series data. 
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
#' @param class The number of the class which need to be oversampled, starting from the class with least observations, 
#'              with the default setting to as most as possible. 
#' @param ratio Targeted positive samples number to achieve/negative samples number, with the default value 1
#' @param Per Percentage of the mixing between ESPO and ADASYN, with the default value 0.8
#' @param R An scalar ratio to tell in which level (towards the boundary) we shall push our syntactic data in ESPO, 
#'          with the default value 1
#' @param k k-NN used in the ADASYN algorithm, with the default value 5
#' @param m m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15
#' @param parallel Whether to run in parallel, with the default setting TRUE. 
#'                 (Recommend for dataset with over 30,000 records. The using cores is 1 less than the total cores)
#' @param progBar Whether to include progress bars, with the default setting TRUE.
#'                For ESPO approach, the bar charactor is |--------|100\%. 
#'                For ADASYN approach, the bar charactor is |========|100\%.
#' @return sample: the time series sequences data oversampled
#' @return label: the label corresponding to each row of records
#' @importFrom stats na.omit 
#' @export OSTSC
#' @examples
#' # This is a simple example to show the usage. A more complex example is inside the vignette.
#' # loading data
#' data(dataset_synthetic_control)
#' # get split feature and label data 
#' train_label <- dataset_synthetic_control$train_y
#' train_sample <- dataset_synthetic_control$train_x
#' # the first dimension of feature and label shall be the same
#' # the second dimention of feature is the time sequence length
#' dim(train_sample)
#' dim(train_label)
#' # check the imbalance of the data
#' table(train_label)
#' # oversample the class 1 to the same amount of class 0
#' MyData <- OSTSC(train_sample, train_label, parallel = FALSE)
#' # store the feature data after oversampling
#' x <- MyData$sample
#' # store the label data after oversampling
#' y <- MyData$label
#' # check the imbalance of the data
#' table(y)

OSTSC <- function(sample, label, class, ratio = 1, Per = 0.8, R = 1, k = 5, m = 15, parallel = TRUE, progBar = TRUE) {
  # Oversample a time series sequence imbalance data.
  #
  # Args:
  #   sample:       Univariate sequence data samples.
  #   label:        Labels corresponding to samples.
  #   class:        The number of the class which need to be oversampled, starting from the class with least observations,
  #                 with the default setting to as most as possible. 
  #   ratio:        Targeted positive samples number to achieve/negative samples number, 
  #                 with the default value 1.
  #   Per:          Percentage of the mixing between ESPO and ADASYN, with the default value 0.8.
  #   R:            An scalar ratio to tell in which level (towards the boundary) we shall push our 
  #                 syntactic data in ESPO, with the default value 1.
  #   k:            k-NN used in the ADASYN algorithm, with the default value 5.
  #   m:            m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15.
  #   parallel:     Whether to run in parallel, with the default setting TRUE. (Recommend for dataset with over 30,000 records)
  #   progBar:      Whether to include progress bars, with the default setting TRUE.
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
    stop ("Number of time series sequences provided in sample do not match the number of classes provided 
          in label. Check dimensions.")
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
  if (!is.numeric(Per)) {
    stop ("The parameter Per is not in correct format, which must be a numeric value.")
  } 
  
  # check if the R input is in the numeric format
  if (!is.numeric(R)) {
    stop ("The parameter R is not in correct format, which must be a numeric value.")
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
  
  # check if the Per input is only one element
  if (length(Per) != 1) {
    stop ("The parameter Per is not in correct format, which must be a single value.")
  }
    
  # check if the R input is only one element
  if (length(R) != 1) {
    stop ("The parameter R is not in correct format, which must be a single value.")
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
    stop ("The parameter ratio is not in correct range, which must be betwwen 0 to 1, including 1.")
  }
    
  # check if the Percentage input is in range [0,1]
  if (Per > 1 || Per < 0) {
    stop ("The parameter Per is not in correct range, which must be betwwen 0 to 1, including 0 and 1.")
  }
    
  # check if the R input is in range [1,+oo)
  if (R < 1) {
    stop ("The parameter R is not in correct range, which must be larger or equal to 1.")
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
    stop ("The parameter parallel is not in correct format, which must be a boolean value.")
  }
  
  # check if the progBar input is a boolean value
  if (!(identical(progBar, FALSE) || identical(progBar, TRUE))) {
    stop ("The parameter progBar is not in correct format, which must be a boolean value.")
  }
  
  # combine labels and features
  fullData <- cbind(label, sample)
  fullData <- matrix(unlist(fullData, use.names = FALSE), ncol = ncol(fullData))
  
  # clean missing values and non-number values by removing their belonging rows
  fullData <- matrix(suppressWarnings(as.numeric(fullData)), nrow = nrow(fullData))
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
    stop ("The input dataset is balanced. No need to do oversample.")
  }
  
  if (missing(class)) {
    class <- count
  } 
  
  if (count < class) {
    warning ("No enough minority classes, class number need to be oversampled is set to ", count)
  }
  
  myData <- list()
  for (i in 1:class) {
    target_class <- as.numeric(as.vector(claTab$Lab[i]))
    newData <- ReguCovar(cleanData, target_class, ratio, R, Per, k, m, parallel, progBar)
    myData <- rbind(myData, newData)
  }
  
  nData <- list()
  for (i in (class + 1):dim(claTab)[1]) {
    target_class <- as.numeric(as.vector(claTab$Lab[i]))
    nega <- cleanData[which(cleanData[, c(1)] == target_class), ]
    nData <- rbind(nData, nega)
  }
  
  # form data
  data_new <- rbind(myData, nData)
  data_new <- matrix(unlist(data_new), ncol=ncol(data_new))
  
  data_x <- data_new[, -1]
  data_y <- data_new[, c(1)]
  data_list <- list("sample" = data_x, "label" = data_y)
  
  return(data_list)
}



