#' kmedians
#'
#' Groups the points in your dataset ,X, into the desired number of clusters, based on the median distance between the points.
#' This function uses random intilization to assign the first medians and then will update the medians and
#' the group assignments until the assignment does not change.
#'
#' @param X a matrix, the dataset being clustered
#' @param num_clusters integer, the desired number of clusters
#' @param n_it integer, number of iterations
#'
#' @return list, contains both medians and labes
#'         medians: matrix
#'         The coordinates of the medians for each cluster
#'
#'         labels: list
#'          List that has the assignment of the cluster for each point in the dataset
#' @export
#'
#' @examples
#'
#'
#'
#'
kmedians <- function(X, num_clusters,n_it=100){

  set.seed(123)
  n <- nrow(X)
  u <- matrix(0, nrow = num_clusters, ncol = n)

  # initialize median points
  medians <- X[sample(n,size=num_clusters,replace=FALSE),]

  for (i in 1:n_it){

    K <- nrow(medians)
    N <- nrow(X)

    old_medians <- medians

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

    if (identical(medians,old_medians)){
      break
    }
  }
  # make the output as a list
  return(list(medians,labels))
}

