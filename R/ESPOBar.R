#' Generate samples by ESPO algorithm.
#' 
#' @param Me Mean vector of positive class
#' @param V Eigen axes matrix (Each axis is a column vector)
#' @param D Modified Eigen Spectrum Value
#' @param P The minority class samples
#' @param N The majority class samples
#' @param R An scalar ratio to tell in which level (towards the boundary) we shall push our syntactic data, 
#'          with the default value 1
#' @param M Scalar tells the reliable portion of the eigen spectrum
#' @param NumToGen The number of samples to be generated
#' @return sample_espo
#' @importFrom fields rdist 
#' @importFrom MASS mvrnorm
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @keywords internal

ESPOBar <- function(Me, V, D, P, N, R, M, NumToGen) {
  # Generate samples by ESPO.
  #
  # Args:
  #   Me:       Mean vector of positive class.
  #   V:        Eigen axes matrix (Each axis is a column vector).
  #   D:        Modified Eigen Spectrum Value.
  #   P:        The minority class samples.
  #   N:        The majority class samples. P and N must have the same feature dimention, greater than one,
  #             with no missing values.
  #   R:        An scalar ratio to tell in which level (towards the boundary) we shall push our syntactic data,
  #             with the default value 1.
  #   M:        Scalar tells the reliable portion of the eigen spectrum.
  #   NumToGen: The number of samples to be generated.
  #
  # Returns:
  #   The ESPO oversampled dataset sample_espo.
  Rn <- M  # reliable portion of the eigen spectrum
  Un <- length(Me) - M  # unreliable portion of the eigen spectrum
  
  MuR <- matrix(0, 1, Rn)  # mean
  SigmaR <- diag(1, Rn)  # standard deviation
  
  MuU <- matrix(0, 1, Un)  # mean
  SigmaU <- diag(1, Un)  # standard deviation
  
  SampGen <- matrix(0, NumToGen * R, length(Me))  # total samples needed be created
  SampSel <- matrix(0, NumToGen, length(Me))  # total samples would be kept
  Prob <- matrix(0, NumToGen*R, 1)  # probability of each sample to be kept
  
  DD <- sqrt(D)  # square root of modified eigen spectrum value
  cat("Oversampling by ESPO: \n")
  nGener <- R * NumToGen  # number of total samples needed be created
  pb <- txtProgressBar(min = 0, max = nGener, style = 3, char = "-")  # progress bar
  for (cnt in 1:nGener) {
    flag = TRUE
    #  genetare new positive data sequence until accepted upon Euclidean distance checking
    while (flag) {
      aR <- mvrnorm(1, MuR, SigmaR)  # generate random vectors from the multivariate normal distribution
      tp <- exp(-0.5*sum(aR^2) - length(aR)*log(2*pi)/2)  # the density of the multivariate normal distribution
    
      if (Un > 0) {
        aU <- mvrnorm(1, MuU, SigmaU)
        a <- c(aR, aU)*DD  # The vector in Eigen transformed domain
      } else {
        a <- aR*DD
      }
      x <- a %*% t(V) + Me  # the modified generated vector
    
      PDist <- rdist(x, P)  # the Euclidean distance between x and positive data
      NDist <- rdist(x, N)  # the Euclidean distance between x and negative data
    
      tmp <- min(NDist)  # the value of the smallest element in the Euclidean distance between x and negative data
      ind <- which.min(NDist)  # the index of the smallest element in the Euclidean distance between x and negative data
    
      # check if to keep the generated vector upon the Euclidean distance between negative and positive data
      if (min(PDist) < tmp) {
        PPDist <- rdist(t(N[ind, ]), P)
        if (tmp >= min(PPDist) && tmp <= max(PPDist)) {
          flag = FALSE
          SampGen[cnt, ] <- x
          Prob[cnt, 1] <- tp
        }
      }
    }
    setTxtProgressBar(pb, cnt)
  }
  close(pb)  # end progress bar

  # upon the density of the multivariate normal distribution, extract samples from generated ones by given R ratio
  for (i in 1:NumToGen) {
    # tmp <- min(Prob)
    ind <- which.min(Prob)
    Prob[ind] <- Inf
    SampSel[i, ] <- SampGen[ind, ]
  }
  
  # form new dataset
  sample_espo <- rbind(SampSel, P)
  return(sample_espo)
}
