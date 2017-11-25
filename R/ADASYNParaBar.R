#' Generate samples by ADASYN approach.
#' 
#' @param p minority class samples
#' @param n majority class samples
#' @param nTarget the targeted number of samples to achieve
#' @param k number of nearest neighbours in k-NN search used by the ADASYN algorithm, with the default value of 5
#' @param m seeds from the positive class in m-NN of the ADASYN algorithm, with the default value of 15
#' @return sampleADA
#' @importFrom fields rdist 
#' @importFrom stats runif 
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @keywords internal

ADASYNParaBar <- function(p, n, nTarget, k, m) {
  # Generate samples by ADASYN.
  #
  # Args:
  #   p:       The minority class samples.
  #   n:       The majority class samples. P and N must have the same feature dimention, greater than one,
  #            with no missing values.
  #   nTarget: The targeted number of samples to achieve.
  #   k:       k-NN used in the ADASYN algorithm, with the default value of 5.
  #   m:       m-NN used in ADASYN, finding seeds from the Positive Class, with the default value of 15.
  #
  # Returns:
  #   The ADASYN oversampled dataset sampleADA.
  nt <- ncol(p)  # number of samples in p
  if (nt == 0) {
    stop ("The minority class is empty")
  } else if (nt == 1) {
    sampleADA <- kronecker(matrix(1, 1, nTarget), p)  # duplicate
  } else {
    cat("Oversampling by ADASYN: \n")
    if (k > nt-1) {
      k <- nt-1  # number of nearest neighbours can not be greater than nt-1
      warning ("The minority class instances is not enough. k is set to ", k)
    } 
    
    numAtt <- nrow(p)  # Feature dimension
    ratio <- FindRatioPara(p, n, m)  # the ratio of each positive sample need to be duplicated
    no <- round(nTarget*ratio)  # the number of each positive sample need to be duplicated
    # adjust No to make the total number of new created samples to equal to the number needed
    while (sum(no) != nTarget) {
      # tmp <- max(No)
      ind <- which.max(no)
      diff <- nTarget - sum(no)
      if (no[ind] + diff > 0) {
        no[ind] <- no[ind] + diff
      } else {
        no[ind] <- 0
      }
    }
    # data generation
    nlen <- length(no)  # number of positive samples 
    i <- 0
    cl <- makeCluster(detectCores(logical = FALSE) - 1)  # start parallel
    registerDoSNOW(cl)
    pb <- txtProgressBar(min = 0, max = nlen, style = 3)  # progress bar
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    # registerDoParallel(cl)
    sampleADA <- foreach(i = 1:nlen, .combine = 'cbind', .options.snow = opts) %dopar% {
      if (no[i] != 0) {
        # k-NN
        d <- rdist(t(p[, i]), t(p))  # the Euclidean distance between each positive sample and other positive data
        d[i] <-Inf  # Set d[i] to infinity manually
        # Find the k indices corresponding to the closest indices
        if (k<log(nt)) {
          minId <- list()
          for (j in 1:k) {
            # tmp <- min(d)
            id <- which.min(d)
            d[id] <-Inf
            minId <- cbind(minId, id)  # sort>=O(n*logn),so we take min: O(n).total time:O(k*n)
          } 
        }else {
          # tmp <- sort(d)
          id <- order(d)
          minId <- id[1:k]
        }
        
        rn <- floor(runif(no[i], min = 0, max = k)) + 1  # random generated No[i] elements integer vector in range 1 to k
        id <- minId[rn]
        weight <- matrix(runif(numAtt * no[i]), nrow = numAtt, ncol = no[i], byrow = TRUE)
        kro <- kronecker(matrix(1, 1, no[i]), p[, i])
        
        # for numeric attributes
        aid <- 1:numAtt
        kro[aid, ] <- kro[aid, ] + weight[aid, ]*(p[aid, unlist(id)] - kro[aid, ])
        
        return(kro)
      }
    }
    close(pb)  # close progress bar
    stopCluster(cl)  # end parallel
  }
  return(sampleADA)
}
