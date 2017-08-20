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
  #   ratio: the ratio of each positive sample need to be duplicated.
  C <- cbind(P, N)  # Note that here columns of P and N are the samples
  poscnt <- ncol(P)  # Number of positive records
  
  #   The ratio of each positive sample need to be duplicated
  cl <- makeCluster(detectCores(logical = FALSE) - 1)  # start parallel
  registerDoParallel(cl, cores = cores)
  seq <- foreach(i = 1:poscnt, .combine = 'rbind') %dopar% {
    d <- rdist(t(P[, i]), t(C))  # the Euclidean distance between each positive sample and full data
    d[i] <- Inf
    # find m number of smallest elements from the Euclidean distance between each positive sample and full data
    min_id <- matrix(0, m, 1)
    for (j in 1:m) {
      tmp <- min(d)
      id <- which.min(d)
      d[id] <- Inf
      min_id[j] <- id  # sort>=O(n*logn),so we take min: O(n).total time:O(k*n)
    }
    Ind <- which(min_id > poscnt)  # find the number of this m number elements larger than the number of positive records
    return(length(Ind))
  }  
  stopCluster(cl)  # end parallel  
  ratio <- matrix(unlist(seq), ncol = 1, byrow = TRUE)
  ratio <- ratio/sum(ratio)
  
  return(ratio)
}
