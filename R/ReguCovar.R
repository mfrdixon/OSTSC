#' Generate samples by ESPO and ADASYN.
#' 
#' @param cleanData First column stores the label data. The other columns store the sample data (without NA, NaN values)
#' @param targetClass The class to be oversampled
#' @param ratio The oversampling ratio 
#'              number (>=1) (default = 1)
#' @param per Ratio of weighting between ESPO and ADASYN (default = 0.8) 
#' @param r A scalar ratio specifying which level (towards the boundary) we shall 
#'          push the synthetic data (in ESPO, default = 1)
#' @param k Number of nearest neighbours in k-NN (for ADASYN) algorithm (default = 5)
#' @param m Seeds from the positive class in m-NN (for ADASYN) algorithm (default = 15) 
#' @param parallel Whether to execute in parallel mode (default = TRUE). 
#'                 (Recommended for datasets with over 30,000 records.)
#' @param progBar Whether to include progress bars (default = TRUE).
#'                For ESPO approach, the bar charactor is |--------|100\%. 
#'                For ADASYN approach, the bar charactor is |========|100\%.
#' @return newData
#' @importFrom stats cov
#' @keywords internal

ReguCovar <- function(cleanData, targetClass, ratio, r, per, k, m, parallel, progBar) {
  # Generate samples by ESPO and ADASYN.
  #
  # Args:
  #   cleanData:    First column is label data, rest is sample data, without missing values.
  #   targetClass: The class to be oversampled. 
  #   ratio:        The oversampling ratio 
  #              number (>=1) (default = 1)  
  #   r:            A scalar ratio specifying which level (towards the boundary) we shall push the synthetic data (in EPSO, default = 1) 
  #   per:          Ratio of weighting between ESPO and ADASYN (default = 0.8)  
  #   k:            k Number of nearest neighbours in k-NN (for ADASYN) algorithm (default = 5)
  #   m:            m Seeds from the positive class in m-NN (for ADASYN) algorithm (default = 15)
  #   parallel:     parallel Whether to execute in parallel mode (default = TRUE). 
  #                 (Recommended for datasets with over 30,000 records.)
  #   progBar:      Whether to include progress bars (default = TRUE).
  #  Returns:
  #   newData: the oversampled dataset.
  
  # form positive (target class) and negative data
  # The negative data is formed using a one-vs-rest strategy.
  positive <- cleanData[which(cleanData[, c(1)] == targetClass), ]
  
  negative <- cleanData[which(cleanData[, c(1)] != targetClass), ]
  
  p <- positive[, -1]  # remove label column
  n <- negative[, -1]
  
  # Number of sequences to be created
  nTarget <- nrow(n)*ratio
  
  poscnt <- nrow(p)
  if (nTarget > poscnt) { 
    # check if the positive data records have already more than the required number of records to be created
    
    # Compute Regularized Eigen Spectra
    numToGen <- ceiling((nTarget - poscnt)*per)
    numADASYN <- nTarget - poscnt - numToGen
    
    me <- apply(p, 2, mean)  # Mean vector of p
    pCov <- cov(p)  # vector covariance
    v <- eigen(pCov)$vectors  # Eigen axes matrix
    # v <- v[, n:1]
    d <- eigen(pCov)$values  # Eigenvalues
    # d <- d[n:1]
    numD <- ncol(p)  # The feature dimension
    ind <- which(d <= 0.005)  # The unreliable eigenvalues
    if (length(ind) != 0) {
      por <- ind[1]  # [1,por] the portion of reliable
    } else {
      por <- numD
    }
    tCov  <- cov(rbind(p, n))  # The covariance matrix of the total data (column)
    dT <- crossprod(v, tCov) %*% v  # dT = v' * tCov * v
    dT <- diag(dT)  # Turning the diagonal of matrix dT to a vector
    
    # Modify the Eigen spectrum according to a 1-Parameter Model
    # dMod: Modified Eigen Spectrum Value
    dMod <- matrix(0, 1, numD)
    alpha <- d[1]*d[por]*(por-1)/(d[1] - d[por])
    beta  <- (por*d[por] - d[1])/(d[1] - d[por])
    for (i in 1:numD) {
      if (i < por) {
        dMod[i] <- d[i]
      } else {
        dMod[i] = alpha/(i+beta)
        if (dMod[i] > dT[i]) {
          dMod[i] <- dT[i]
        }
      }
    }
    # Create Oversampled Data by ESPO and ADASYN
    # Users choose if applying in parallel and if adding progress bar
    if (numToGen != 0) {
      if (identical(parallel, FALSE)) {
        if (identical(progBar, FALSE)) {
          sampleESPO <- ESPO(me, v, dMod, p, n, r, por, numToGen)
        } else {
          cat("Oversampling class", targetClass, "... \n")
          sampleESPO <- ESPOBar(me, v, dMod, p, n, r, por, numToGen)
        }
      } else {
        if (identical(progBar, FALSE)) {
          sampleESPO <- ESPOPara(me, v, dMod, p, n, r, por, numToGen)
        } else {
          cat("Oversampling class", targetClass, "... \n")
          sampleESPO <- ESPOParaBar(me, v, dMod, p, n, r, por, numToGen)
        }
      }
    }
    
    if (numADASYN != 0) {
      if (identical(parallel, FALSE)) {
        if (identical(progBar, FALSE)) {
          sampleADA <- ADASYN(t(p), t(n), numADASYN, k, m)
        } else {
          sampleADA <- ADASYNBar(t(p), t(n), numADASYN, k, m)
        }
      } else {
        if (identical(progBar, FALSE)) {
          sampleADA <- ADASYNPara(t(p), t(n), numADASYN, k, m)
        } else {
          sampleADA <- ADASYNParaBar(t(p), t(n), numADASYN, k, m)
        }
      }
    }
    
    # Form new data
    dataTargetClass <- rbind(t(sampleADA), sampleESPO)
    newData <- cbind(matrix(targetClass, nTarget, 1), dataTargetClass)
    return(newData)
  } else {
    return(positive)
  }
}
