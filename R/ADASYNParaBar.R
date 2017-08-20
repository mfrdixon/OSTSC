#' Generate samples by ADASYN approach.
#' 
#' @param P minority class samples
#' @param N majority class samples
#' @param nTarget The targeted number of samples to achieve
#' @param k k-NN used in the ADASYN algorithm, with the default value 5
#' @param m m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15
#' @return sample_ada
#' @importFrom fields rdist 
#' @importFrom stats runif 
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @keywords internal

ADASYNParaBar <- function(P, N, nTarget, k, m) {
  # Generate samples by ADASYN.
  #
  # Args:
  #   P:       The minority class samples.
  #   N:       The majority class samples. P and N must have the same feature dimention, greater than one,
  #            with no missing values.
  #   nTarget: The targeted number of samples to achieve.
  #   k:       k-NN used in the ADASYN algorithm, with the default value 5.
  #   m:       m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15.
  #
  # Returns:
  #   The ADASYN oversampled dataset sample_ada.
  NT <- ncol(P)  # NT is number of samples in P
  if (NT == 0) {
    stop ("The minority class is empty")
  } else if (NT == 1) {
    sample_ada <- kronecker(matrix(1, 1, nTarget), P)  # duplicate
  } else {
    cat("Oversampling by ADASYN: \n")
    if (k > NT-1) {
      k <- NT-1  # number of nearest neighbours can not be greater than NT-1
      warning ("The minority class instances is not enough. k is set to ", k)
    } 
    
    NumAtt <- nrow(P)  # Feature dimension
    ratio <- FindRatioPara(P, N, m)
    No <- round(nTarget*ratio)
    while (sum(No) != nTarget) {
      tmp <- max(No)
      ind <- which.max(No)
      diff <- nTarget - sum(No)
      if (No[ind] + diff > 0) {
        No[ind] <- No[ind] + diff
      } else {
        No[ind] <- 0
      }
    }
    # generation
    nlen <- length(No)
    
    cl <- makeCluster(detectCores(logical = FALSE) - 1)
    registerDoSNOW(cl)
    pb <- txtProgressBar(min = 0, max = nlen, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    # registerDoParallel(cl, cores = cores)
    sample_ada <- foreach(i = 1:nlen, .combine = 'cbind', .options.snow = opts) %dopar% {
      if (No[i] != 0) {
        # k-NN
        d <- rdist(t(P[, i]), t(P))
        d[i] <-Inf  # Set d[i] to infinity manually
        # Find the k indices corresponding to the closest indices
        if (k<log(NT)) {
          min_id <- list()
          for (j in 1:k) {
            tmp <- min(d)
            id <- which.min(d)
            d[id] <-Inf
            min_id <- cbind(min_id, id)  # sort>=O(n*logn),so we take min: O(n).total time:O(k*n)
          } 
        }else {
          tmp <- sort(d)
          id <- order(d)
          min_id <- id[1:k]
        }
        
        rn <- floor(runif(No[i], min=0, max=k)) + 1
        id <- min_id[rn]
        weight <- matrix(runif(NumAtt*No[i]), nrow=NumAtt, ncol=No[i], byrow = TRUE)
        D <- kronecker(matrix(1, 1, No[i]), P[, i])
        
        # for numeric attributes
        aid <- 1:NumAtt
        D[aid, ] <- D[aid, ] + weight[aid, ]*(P[aid, unlist(id)] - D[aid, ])
        
        return(D)
      }
    }
    close(pb)
    stopCluster(cl)
  }
  return(sample_ada)
}
