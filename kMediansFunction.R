library(flexclust)


kmedian <- function(x, n_cluster, n_it=100){
  
  # A quick and (really) dirty implementation of K-Medians
  
  #Parameters:
  #-----------
  #x: 2D numpy array of data;
  #n_cluster: desired number of clusters
  
  #Output:
  #-------
  #medians: 2D numpy array of the medians
  #labels: 1D numpy array of labels
  n <- nrow(x)
  #u <- matrix(0, nrow = n_cluster, ncol = n)
  medians <- x[sample(nrow(x),size=n_cluster,replace=FALSE),]
  for (i in 1:n_it){
    u <- matrix(0, nrow = n_cluster, ncol = n)
    dist <- dist2(x, medians, "manhattan", p=1)
    labels <- apply(d, 1, which.min)
    for (j in 1:n){
      u[labels[j], j] <- 1
    }
    for (k in 1:n_cluster){
      medians[k,] = apply((matrix(x[u[k,]==1],ncol=2)), 2, median)
    }
  }
  return(medians)
}


B<-matrix(rexp(40, rate=.1), ncol=2)
B
kmedian(B,3, n_it=100)

A <- matrix( 
  c(2, 4, 3, 1, 5, 7, 1, 0), # the data elements 
  nrow=4,              # number of rows 
  ncol=2,              # number of columns 
  byrow = TRUE)   
nrow(A)
m<-A[sample(nrow(A),size=3,replace=FALSE),]
m
for (i in 1:10){
  print(i)
}
d <- dist2(A, m, method = "manhattan", p=1)
d
m
A
c <- apply(d, 1, which.min)
index <- matrix(0, nrow = 3, ncol = 4)
for (i in 1:nrow(A)){
  index[c[i], i] <- 1
}
index
for (i in 1:nrow(A)){
  print(i)
}
for (k in 1:3){
  m[k,] <- apply((matrix(A[index[k,]==1],ncol=2)), 2, median)
}
m
matrix(A[index[1,]==1],ncol=2)
apply((matrix(A[index[3,]==1],ncol=2)), 2, median)
m
