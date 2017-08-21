#' Generate samples by ESPO and ADASYN.
#' 
#' @param P Minority class samples
#' @param N Majority class samples
#' @param nTarget The targeted number of samples to achieve
#' @param Per Percentage of the mixing between ESPO and ADASYN, with the default value 0.8
#' @param R An scalar ratio to tell in which level (towards the boundary) we shall push our syntactic data, 
#'          with the default value 1
#' @param k k-NN used in the ADASYN algorithm, with the default value 5
#' @param m m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15
#' @param parallel Whether to run in parallel, with the default setting TRUE (Recommend for dataset with over 30,000 records)
#' @param progBar Whether to include progress bars, with the default setting TRUE
#' @return myData
#' @importFrom stats cov
#' @keywords internal

ReguCovar <- function(P, N, nTarget, R, Per, k, m, parallel, progBar) {
  # Generate samples by ESPO and ADASYN.
  #
  # Args:
  #   P:        Minority class samples.
  #   N:        Majority class samples. P and N must have the same feature dimention, greater than one,
  #             with no missing values.
  #   nTarget:  The targeted number of samples to achieve.
  #   R:        An scalar ratio to tell in which level (towards the boundary) we shall push our syntactic data,
  #             with the default value 1. 
  #   Per:      Percentage of the mixing between ESPO and ADASYN. 
  #   k:        k-NN used in the ADASYN algorithm, with the default value 5.
  #   m:        m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15.
  #   parallel: Whether to run in parallel, with the default setting TRUE. (Recommend for dataset with over 30,000 records)
  #   progBar:  Whether to include progress bars, with the default setting TRUE.
  #
  # Returns:
  #   myData: the oversampled dataset.
  
  # check if the positive data records have already more than the records asked to be created
  poscnt <- nrow(P)
  if (nTarget < poscnt) {
    stop ("The targeted positive class size is too small compared with the existing size")
  }
  
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
        sample_espo <- ESPOBar(Me, V, dMod, P, N, R, M, NumToGen)
      }
    } else {
      if (identical(progBar, FALSE)) {
        sample_espo <- ESPOPara(Me, V, dMod, P, N, R, M, NumToGen)
      } else {
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
  myData <- rbind(t(sample_ada), sample_espo)
  return(myData)
}
