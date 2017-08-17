#' Generate samples by EPSO algorithm.
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
#' @return sample_epso
#' @importFrom fields rdist 
#' @importFrom MASS mvrnorm
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom foreach foreach %dopar%
#' @importFrom doSNOW registerDoSNOW
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @keywords internal

EPSO <- function(Me, V, D, P, N, R, M, NumToGen) {
  # Generate samples by EPSO.
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
  #   The EPSO oversampled dataset sample_epso.
  Rn <- M
  Un <- length(Me) - M
  
  MuR <- matrix(0, 1, Rn)
  SigmaR <- diag(1, Rn)
  
  MuU <- matrix(0, 1, Un)
  SigmaU <- diag(1, Un)
  
  SampSel <- matrix(0, NumToGen, length(Me))
  
  cnt <- 0
  DD <- sqrt(D)
  
  nGener <- R * NumToGen
  
  cl <- makeCluster(detectCores(logical = FALSE) - 1)
  registerDoSNOW(cl)
  pb <- txtProgressBar(min = 0, max = nGener, style = 3, char = "-")
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  # registerDoParallel(cl, cores = cores)
  seq <- foreach(cnt = 1:nGener, .combine = 'rbind', .options.snow = opts) %dopar% {
    flag = TRUE
    while (flag) {

      aR <- MASS::mvrnorm(1, MuR, SigmaR)
      tp <- exp(-0.5*sum(aR^2) - length(aR)*log(2*pi)/2)
      
      if (Un > 0) {
        aU <- MASS::mvrnorm(1, MuU, SigmaU)
        a <- c(aR, aU)*DD  # The vector in Eigen transformed domain
      } else {
        a <- aR*DD
      }
      x <- a %*% t(V) + Me
      
      PDist <- fields::rdist(x, P)
      NDist <- fields::rdist(x, N)
      
      tmp <- min(NDist)
      ind <- which.min(NDist)
      
      if (min(PDist) < tmp) {
        PPDist <- fields::rdist(t(N[ind, ]), P)
        if (tmp >= min(PPDist) && tmp <= max(PPDist)) {
          res <- list("x" = x, "tp" = tp)
          flag = FALSE
          return(res)
        }
      }
    }
  }
  close(pb)
  stopCluster(cl)
  
  SampGen <- matrix(unlist(seq[, 1]), ncol = length(Me), byrow = TRUE)
  Prob <- matrix(unlist(seq[, 2]), ncol = 1, byrow = TRUE)

  for (i in 1:NumToGen) {
    tmp <- min(Prob)
    ind <- which.min(Prob)
    Prob[ind] <- Inf
    SampSel[i, ] <- SampGen[ind, ]
  }
  sample_epso <- rbind(SampSel, P)
  return(sample_epso)
}
