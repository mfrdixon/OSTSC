#' Generate samples by ESPO and ADASYN.
#' 
#' @param cleanData First column is label data, rest is sample data, without NA, NaN values
#' @param target_class The class needs to be oversampled
#' @param ratio Targeted positive samples number to achieve/negative samples number, with the default value 1.
#' @param Per Percentage of the mixing between ESPO and ADASYN, with the default value 0.8
#' @param R An scalar ratio to tell in which level (towards the boundary) we shall push our syntactic data, 
#'          with the default value 1
#' @param k k-NN used in the ADASYN algorithm, with the default value 5
#' @param m m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15
#' @param parallel Whether to run in parallel, with the default setting TRUE 
#'                 (Recommend for dataset with over 30,000 records)
#' @param progBar Whether to include progress bars, with the default setting TRUE
#' @return newData
#' @importFrom stats cov
#' @keywords internal

ReguCovar <- function(cleanData, target_class, ratio, R, Per, k, m, parallel, progBar) {
  # Generate samples by ESPO and ADASYN.
  #
  # Args:
  #   cleanData:    First column is label data, rest is sample data, without missing values.
  #   target_class: The class needs to be oversampled. 
  #   ratio:        Targeted positive samples number to achieve/negative samples number, 
  #                 with the default value 1.
  #   R:            An scalar ratio to tell in which level (towards the boundary) we shall push our 
  #                 syntactic data, with the default value 1. 
  #   Per:          Percentage of the mixing between ESPO and ADASYN. 
  #   k:            k-NN used in the ADASYN algorithm, with the default value 5.
  #   m:            m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15.
  #   parallel:     Whether to run in parallel, with the default setting TRUE. 
  #                 (Recommend for dataset with over 30,000 records)
  #   progBar:      Whether to include progress bars, with the default setting TRUE.
  #
  # Returns:
  #   newData: the oversampled dataset.
  
  # form positive (target class) data and negative data
  # The negative data is formed using a one-vs-rest manner.
  Positive <- cleanData[which(cleanData[, c(1)] == target_class), ]
  
  Negative <- cleanData[which(cleanData[, c(1)] != target_class), ]
  
  P <- Positive[, -1]  # remove label column
  N <- Negative[, -1]
  
  # Number of sequences needed to be created
  nTarget <- nrow(N)*ratio
  
  poscnt <- nrow(P)
  if (nTarget > poscnt) { 
    # check if the positive data records have already more than the records asked to be created
    
    # Compute Regularized Eigen Spectra
    NumToGen <- ceiling((nTarget - poscnt)*Per)
    NumADASYN <- nTarget - poscnt - NumToGen
    
    Me <- apply(P, 2, mean)  # Mean vector of P
    PCov <- cov(P)  # vector covariance
    V <- eigen(PCov)$vectors  # Eigen axes matrix
    # V <- V[, n:1]
    D <- eigen(PCov)$values  # Eigenvalues
    # D <- D[n:1]
    n <- ncol(P)  # The feature dimension
    Ind <- which(D <= 0.005)  # The unreliable eigenvalues
    if (length(Ind) != 0) {
      M <- Ind[1]  # [1,M] the portion of reliable
    } else {
      M <- n
    }
    TCov  <- cov(rbind(P, N))  # The covariance matrix of the total data (column)
    dT <- crossprod(V, TCov) %*% V  # dT = V' * TCov * V
    dT <- diag(dT)  # Turning the diagonal of matrix dT to a vector
    
    # Modify the Eigen spectrum according to a 1-Parameter Model
    # dMod: Modified Eigen Spectrum Value
    dMod <- matrix(0, 1, n)
    Alpha <- D[1]*D[M]*(M-1)/(D[1] - D[M])
    Beta  <- (M*D[M] - D[1])/(D[1] - D[M])
    for (i in 1:n) {
      if (i < M) {
        dMod[i] <- D[i]
      } else {
        dMod[i] = Alpha/(i+Beta)
        if (dMod[i] > dT[i]) {
          dMod[i] <- dT[i]
        }
      }
    }
    # Create Oversampled Data by ESPO and ADASYN, users choose if applying in parallel and if adding progress bar
    if (NumToGen != 0) {
      if (identical(parallel, FALSE)) {
        if (identical(progBar, FALSE)) {
          sample_espo <- ESPO(Me, V, dMod, P, N, R, M, NumToGen)
        } else {
          cat("Oversampling class", target_class, "... \n")
          sample_espo <- ESPOBar(Me, V, dMod, P, N, R, M, NumToGen)
        }
      } else {
        if (identical(progBar, FALSE)) {
          sample_espo <- ESPOPara(Me, V, dMod, P, N, R, M, NumToGen)
        } else {
          cat("Oversampling class", target_class, "... \n")
          sample_espo <- ESPOParaBar(Me, V, dMod, P, N, R, M, NumToGen)
        }
      }
    }
    
    if (NumADASYN != 0) {
      if (identical(parallel, FALSE)) {
        if (identical(progBar, FALSE)) {
          sample_ada <- ADASYN(t(P), t(N), NumADASYN, k, m)
        } else {
          sample_ada <- ADASYNBar(t(P), t(N), NumADASYN, k, m)
        }
      } else {
        if (identical(progBar, FALSE)) {
          sample_ada <- ADASYNPara(t(P), t(N), NumADASYN, k, m)
        } else {
          sample_ada <- ADASYNParaBar(t(P), t(N), NumADASYN, k, m)
        }
      }
    }
    
    # Form new data
    data_target_class <- rbind(t(sample_ada), sample_espo)
    newData <- cbind(matrix(target_class, nTarget, 1), data_target_class)
    return(newData)
  } else {
    return(Positive)
  }
}
