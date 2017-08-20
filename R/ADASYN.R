#' Generate samples by ADASYN approach.
#' 
#' @param P minority class samples
#' @param N majority class samples
#' @param nTarget the targeted number of samples to achieve
#' @param k k-NN used in the ADASYN algorithm, with the default value 5
#' @param m m-NN used in ADASYN, finding seeds from the Positive Class, with the default value 15
#' @return sample_ada
#' @importFrom fields rdist 
#' @importFrom stats runif 
#' @keywords internal

ADASYN <- function(P, N, nTarget, k, m) {
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
  NT <- ncol(P)  # number of samples in positive data
  if (NT == 0) {
    stop ("The minority class is empty")
  } else if (NT == 1) {
    sample_ada <- kronecker(matrix(1, 1, nTarget), P)  # duplicate
  } else {
    if (k > NT-1) {
      k <- NT-1  # number of nearest neighbours can not be greater than NT-1
      warning ("The minority class instances is not enough. k is set to ", k)
    } 
    NumAtt <- nrow(P)  # Feature dimension
    ratio <- FindRatio(P, N, m)  # the ratio of each positive sample need to be duplicated
    No <- round(nTarget*ratio)  # the number of each positive sample need to be duplicated
    # adjust No to make the total number of new created samples to equal to the number needed
    while (sum(No) != nTarget) {  
      # tmp <- max(No)
      ind <- which.max(No)
      diff <- nTarget - sum(No)
      if (No[ind] + diff > 0) {
        No[ind] <- No[ind] + diff
      } else {
        No[ind] <- 0
      }
    }
    # data generation
    sample_ada <- list()
    for (i in 1:length(No)) {
      if (No[i] == 0) {  # jump the positive samples which don't need to be duplicated
        next
      }
      # k-NN
      d <- rdist(t(P[, i]), t(P))  # the Euclidean distance between each positive sample and other positive data
      d[i] <-Inf  # Set d[i] to infinity manually
      # Find the k indices corresponding to the closest indices
      if (k<log(NT)) {
        min_id <- list()
        for (j in 1:k) {
          # tmp <- min(d)
          id <- which.min(d)
          d[id] <-Inf
          min_id <- cbind(min_id, id)  # sort>=O(n*logn),so we take min: O(n).total time:O(k*n)
        } 
      }else {
        # tmp <- sort(d)
        id <- order(d)
        min_id <- id[1:k]
      }
      
      rn <- floor(runif(No[i], min=0, max=k)) + 1  # random generated No[i] elements integer vector in range 1 to k
      id <- min_id[rn]
      weight <- matrix(runif(NumAtt*No[i]), nrow=NumAtt, ncol=No[i], byrow = TRUE)
      D <- kronecker(matrix(1, 1, No[i]), P[, i])
      
      # for numeric attributes
      aid <- 1:NumAtt
      D[aid, ] <- D[aid, ] + weight[aid, ]*(P[aid, unlist(id)] - D[aid, ])
      
      sample_ada <- cbind(sample_ada, D)
    }
  }
  return(sample_ada)
}
