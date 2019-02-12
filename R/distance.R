distance <- function(X, medians){
  # Calculates the Manhanttan distance between the medians and every point in the dataset
  #
  # Parameters
  # ----------
  # x: matrix
  # The dataset being clustered
  #
  # medians: matrix
  # Medians of the clusters
  #
  # Returns
  # -------
  # dist: matrix
  # Distance between each point and each median

  K = ncol(medians)
  n = nrow(X)
  
  matrix<- matrix(nrow=n,ncol=K)
  
  for (k in 1:K) {
    for (i in 1:n){
      matrix[i,k] <- abs(X[i,1]-medians[k,1])+abs(X[i,2]-medians[k,2])
    }
  }  
  
  return (matrix)
}

  
  return (matrix)
}
