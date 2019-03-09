#' distance
#'
#' Calculates the Manhanttan distance between the medians and every point in the dataset
#'
#' @param X a matrix, the dataset being clustered
#' @param medians a matrix, dedians of the clusters
#'
#' @return a matrix, distance between each point and each median
#' @export
#'
#' @examples
#' A <- matrix(
#' c(1,2,3,4,5,6),
#' nrow = 3,
#' ncol = 2,
#' byrow = TRUE)
#' m <- matrix(
#'   c(1,1,2,2),
#'   nrow = 2,
#'   ncol = 2,
#'  byrow = TRUE)
#' distance(A,m)
#'




distance <- function(X, medians){

  # Check that inputs are valid and as expected
  if(!is.matrix(X)) stop("Input X should be a matrix!")

  if(!is.matrix(medians)) stop("Input medians should be a matrix!")

  if(nrow(X)  < (nrow(medians))) stop("non-numeric matrix extent")

  K = nrow(medians)
  n = nrow(X)

  dist<- matrix(nrow=n,ncol=K)

  for (k in 1:K) {
    for (i in 1:n){
      dist[i,k] <- abs(X[i,1]-medians[k,1])+abs(X[i,2]-medians[k,2])
    }
  }

  return (dist)
}



