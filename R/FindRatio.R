#' Find the distribution of the positive data.
#' 
#' @param p minority class samples
#' @param n majority class samples
#' @param m For m-NN used in ADASYN, finding seeds from the Positive Class, with the default value = 15
#' @return ratio
#' @importFrom fields rdist 
#' @keywords internal

FindRatio <- function(p, n, m) {
  # Find the distribution of the positive data to determine the ratio of each positive data to be generated.
  #
  # Args:
  #   p: minority class samples.
  #   n: majority class samples. P and N must have the same feature dimension, greater than one,
  #      with no missing values.
  #   m: m-NN used in finding seeds from the Positive Class, with the default value = 15
  #
  # Returns:
  #   ratio: the ratio of each positive sample need to be duplicated.
  c <- cbind(p, n)  # Note that here columns of P and N are the samples
  poscnt <- ncol(p)  # Number of positive records
  
  # The ratio of each positive sample needed to be duplicated
  ratio <- matrix(0, poscnt, 1)
  for (i in 1:poscnt) {
    d <- rdist(t(p[, i]), t(c))  # the Euclidean distance between each positive sample and full data
    d[i] <- Inf
    # find the indices of m number smallest elements using the Euclidean distance between each positive sample and full data
    minId <- matrix(0, m, 1)
    for (j in 1:m) {
      # tmp <- min(d)
      id <- which.min(d)
      d[id] <- Inf
      minId[j] <- id  # sort>=O(n*logn),so we take min: O(n).total time:O(k*n)
    }
    ind <- which(minId > poscnt)  # find all negative samples from this m number closest elements
    ratio[i] <- length(ind)
  }
  ratio <- ratio/sum(ratio)
  return(ratio)
}
