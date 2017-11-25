#' Generate samples by ESPO algorithm.
#' 
#' @param me Mean vector of positive class
#' @param v Eigen axes matrix (Each axis is a column vector)
#' @param dMod Modified Eigen Spectrum Value
#' @param p The minority class samples
#' @param n The majority class samples
#' @param r A scalar specifying which level (towards the boundary) we shall push the synethetic data, 
#'          with the default value = 1
#' @param m Scalar specifies the reliable portion of the eigen spectrum
#' @param numToGen The number of samples to be generated
#' @return sampleESPO
#' @importFrom fields rdist 
#' @importFrom MASS mvrnorm
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom foreach foreach %dopar%
#' @importFrom doSNOW registerDoSNOW
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @keywords internal

ESPOParaBar <- function(me, v, dMod, p, n, r, m, numToGen) {
  # Generate samples by ESPO.
  #
  # Args:
  #   me:       Mean vector of positive class.
  #   v:        Eigen axes matrix (Each axis is a column vector).
  #   dMod:        Modified Eigen Spectrum Value.
  #   p:        The minority class samples.
  #   n:        The majority class samples. P and N must have the same feature dimention, greater than one,
  #             with no missing values.
  #   r:        A scalar ratio specifying which level (towards the boundary) we shall push the synthetic data,
  #             with the default value = 1.
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
  
  sampSel <- matrix(0, numToGen, length(me))  # total samples would be kept
  
  dd <- sqrt(dMod)  # square root of modified eigen spectrum value
  
  nGener <- r * numToGen  # number of total samples needed be created
  cat("Oversampling by ESPO: \n")
  cl <- makeCluster(detectCores(logical = FALSE) - 1)  # start parallel
  registerDoSNOW(cl)
  pb <- txtProgressBar(min = 0, max = nGener, style = 3, char = "-")  # progress bar
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  # registerDoParallel(cl, cores = cores)
  seq <- foreach(cnt = 1:nGener, .combine = 'rbind', .options.snow = opts) %dopar% {
    flag <- TRUE
    #  generate new positive data sequence until accepted, using Euclidean distance checking
    while (flag) {
      aR <- mvrnorm(1, muR, sigmaR)  # draw random vectors from the multivariate normal distribution
      dens <- exp(-0.5*sum(aR^2) - length(aR)*log(2*pi)/2)  # the density of the multivariate normal distribution
      
      if (un > 0) {
        aU <- mvrnorm(1, muU, sigmaU)
        a <- c(aR, aU)*dd  # The vector in Eigen transformed domain
      } else {
        a <- aR*dd
      }
      x <- a %*% t(v) + me  # the modified generated vector
      
      pDist <- rdist(x, p)  # the Euclidean distance between x and positive data
      nDist <- rdist(x, n)  # the Euclidean distance between x and negative data
      
      val <- min(nDist)  # the value of the smallest element of the Euclidean distances between x and the negative samples
      ind <- which.min(nDist)  # the index of the smallest element of the Euclidean distances between x and the negative samples
      
      # check whether to keep the generated vector using the Euclidean distance between negative and positive data
      if (min(pDist) < val) {
        ppDist <- rdist(t(n[ind, ]), p)
        if (val >= min(ppDist) && val <= max(ppDist)) {
          res <- list("x" = x, "dens" = dens)
          flag <- FALSE
          return(res)
        }
      }
    }
  }
  close(pb)  # end progress bar
  stopCluster(cl)  # end parallel
  
  sampGen <- matrix(unlist(seq[, 1]), ncol = length(me), byrow = TRUE)
  prob <- matrix(unlist(seq[, 2]), ncol = 1, byrow = TRUE)
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
