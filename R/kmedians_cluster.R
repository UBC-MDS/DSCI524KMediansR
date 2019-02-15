kmedians <- function(X, num_clusters, n_it=100){
  # Groups the points in your dataset ,X, into the desired number of clusters, based on the median distance between the points.
  # This function uses random intilization to assign the first medians and then will update the medians and
  # the group assignments until the assignment does not change.
  #
  # Parameters
  # ----------
  # X: matrix
  # The dataset being clustered
  #
  # num_clusters: integer
  # The desired number of clusters
  #
  # n_it: integer
  # The number of loops. Default value is 100.
  #
  # Returns
  # -------
  # List contains both medians and labels :
  #
  #   medians: matrix
  #   The coordinates of the medians for each cluster
  #
  #   labels: list
  #   List that has the assignment of the cluster for each point in the dataset

  set.seed(123)
  n <- nrow(X)
  u <- matrix(0, nrow = num_clusters, ncol = n)

  # initialize median points
  medians <- X[sample(n,size=num_clusters,replace=FALSE),]

  for (i in 1:n_it){

    K <- nrow(medians)
    N <- nrow(X)

    dist <- matrix(nrow=N,ncol=K)

    for (k in 1:K) {
      for (i in 1:n){
        dist[i,k] <- abs(X[i,1]-medians[k,1])+abs(X[i,2]-medians[k,2])
      }
    }

    labels <- apply(dist, 1, which.min)

    for (j in 1:n){
      u[labels[j], j] <- 1
    }

    for (k in 1:num_clusters){
      medians[k,] = apply((matrix(X[u[k,]==1],ncol=2)), 2, median)
    }
  }
  # make the output as a list
  return(list(medians,labels))
}

