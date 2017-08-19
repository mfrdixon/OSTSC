#' Find the distribution of the positive data.
#' 
#' @param P minority class samples
#' @param N majority class samples
#' @param m m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15
#' @return ratio
#' @importFrom fields rdist 
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @keywords internal

FindRatioPara <- function(P, N, m) {
  # Find the distribution of the positive data to determine the ratio of each positive data to be generated.
  #
  # Args:
  #   P: minority class samples.
  #   N: majority class samples. P and N must have the same feature dimention, greater than one,
  #      with no missing values.
  #   m: m-NN used in finding seeds from the Positive Class, with the default value 15
  #
  # Returns:
  #   The ratio of each positive sample need to be duplicated ratio.
  C <- cbind(P, N)  # Note that here columns of P and N are the samples
  poscnt <- ncol(P)
  
  cl <- parallel::makeCluster(parallel::detectCores(logical = FALSE) - 1)
  doParallel::registerDoParallel(cl, cores = cores)

  seq <- foreach(i = 1:poscnt, .combine = 'rbind') %dopar% {
    d <- fields::rdist(t(P[, i]), t(C))
    d[i] <- Inf
    min_id <- matrix(0, m, 1)
    for (j in 1:m) {
      tmp <- min(d)
      id <- which.min(d)
      d[id] <- Inf
      min_id[j] <- id  # sort>=O(n*logn),so we take min: O(n).total time:O(k*n)
    }
    Ind <- which(min_id > poscnt)
    return(length(Ind))
  }
  
  parallel::stopCluster(cl)
  
  ratio <- matrix(unlist(seq), ncol = 1, byrow = TRUE)

  ratio <- ratio/sum(ratio)
  
  return(ratio)
}
