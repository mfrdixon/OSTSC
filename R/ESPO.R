#' Generate samples by ESPO algorithm.
#' 
#' @param me Mean vector of positive class
#' @param v Eigen axes matrix (Each axis is a column vector)
#' @param dMod Modified Eigen Spectrum Value
#' @param p The minority class samples
#' @param n The majority class samples
#' @param r A scalar ratio specifies which level (towards the boundary) we shall push the synthetic data, 
#'          with the default value 1
#' @param m Scalar specifies the reliable portion of the eigen spectrum
#' @param numToGen The number of samples to be generated
#' @return sampleESPO
#' @importFrom fields rdist 
#' @importFrom MASS mvrnorm
#' @keywords internal

ESPO <- function(me, v, dMod, p, n, r, m, numToGen) {
  # Generate samples by ESPO.
  #
  # Args:
  #   me:       Mean vector of positive class.
  #   v:        Eigen axes matrix (Each axis is a column vector).
  #   dMod:        Modified Eigen Spectrum Value.
  #   p:        The minority class samples.
  #   n:        The majority class samples. P and N must have the same feature dimension, greater than one,
  #             with no missing values.
  #   r:        A scalar ratio specifies which level (towards the boundary) we shall push the synthetic data,
  #             with a default value of 1.
  #   m:        Scalar specifies the reliable portion of the eigen spectrum.
  #   numToGen: The number of samples to be generated.
  #
  # Returns:
  #   The ESPO oversampled dataset sampleESPO.
  rn <- m  # reliable portion of the eigen spectrum
  un <- length(me) - m  # unreliable portion of the eigen spectrum
  
  muR <- matrix(0, 1, rn)  # mean
  sigmaR <- diag(1, rn)  # standard deviation
  
  muU <- matrix(0, 1, un)  # mean
  sigmaU <- diag(1, un)  # standard deviation
  
  sampGen <- matrix(0, numToGen * r, length(me))  # total samples needed 
  sampSel <- matrix(0, numToGen, length(me))  # total samples which should be kept
  prob <- matrix(0, numToGen*r, 1)  # probability of each sample to be kept
  
  cnt <- 0
  dd <- sqrt(dMod)  # square root of modified eigen spectrum value
  
  #  generate new positive data sequence until accepted, using Euclidean distance checking
  while (cnt < r * numToGen) {
    aR <- mvrnorm(1, muR, sigmaR)  # generate random vectors from the multivariate normal distribution
    dens <- exp(-0.5*sum(aR^2) - length(aR)*log(2*pi)/2)  # the density of the multivariate normal distribution
    
    if (un > 0) {
      aU <- mvrnorm(1, muU, sigmaU)
      a <- c(aR, aU)*dd  # the vector in eigen transformed domain
    } else {
      a <- aR*dd
    }
    x <- a %*% t(v) + me  # the modified generated vector
    
    pDist <- rdist(x, p)  # the Euclidean distance between x and positive data
    nDist <- rdist(x, n)  # the Euclidean distance between x and negative data
    
    val <- min(nDist)  # the value of the smallest element in the Euclidean distances between x and negative data
    ind <- which.min(nDist)  # the index of the smallest element in the Euclidean distances between x and negative data
    
    # check whether to keep the generated vector using the Euclidean distance between negative and positive samples
    if (min(pDist) < val) {
      ppDist <- rdist(t(n[ind, ]), p)
      if (val >= min(ppDist) && val <= max(ppDist)) {
        cnt <- cnt + 1
        sampGen[cnt, ] <- x
        prob[cnt, 1] <- dens
      }
    }
  }
  
  # Draw samples from a multivariate normal distribution and discard based on the ratio r
  for (i in 1:numToGen) {
    # tmp <- min(Prob)
    ind <- which.min(prob)
    prob[ind] <- Inf
    sampSel[i, ] <- sampGen[ind, ]
  }
  
  # form new dataset
  sampleESPO <- rbind(sampSel, p)
  return(sampleESPO)
}
