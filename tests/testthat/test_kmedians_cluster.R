# kmedians_cluster tests
#
# Testing includes:
# - checks errors of the missing input
# - checks errors of the datatype of the input
# - checks if num_clusters is larger than the data rows
# - checks the datatype of the output
# - checks the size of the output
# - correct medians and clustering of the toy example


library(KMediansR)

context("testing kmedians clustering")

# toy data for calculation
A <- matrix(
  c(1,1,1,2,2,1,100,100,101,100,100,101),
  nrow = 6,
  ncol = 2,
  byrow = TRUE)

n <- 2

test_that("missing input errors", {

  expect_error(kmedians(X = NULL, num_clusters = n),

               'missing X')

  expect_error(kmedians(X = A, num_clusters = NULL),

               'missing num_clusters')

})


test_that("datatype errors", {

  expect_error(kmedians(X = "abcd", num_clusters = n),

               'X should be matrix')

  expect_error(kmedians(X = 100, num_clusters = n),

               'X should be matrix')

  expect_error(kmedians(X = c(1,2), num_clusters = n),

               'X should be matrix')

  expect_error(kmedians(X = A, num_clusters = 1.5),

               'num_clusters should be integer')

  expect_error(kmedians(X = A, num_clusters = "2"),

               'num_clusters should be integer')

  expect_error(kmedians(X = A, num_clusters = 0),

               'num_clusters should not be 0')

})

test_that("error if num_clusters is larger than the data rows", {

  expect_error(kmedians(X = A, num_clusters = nrow(A)+1),

               'num_clusters must be less than or equal to data points')

})


test_that("test if output is a list given toy data", {

  expect_equal(is.list(kmedians(X = A, num_clusters = n)), TRUE)

})


test_that("test size of output given toy data", {

  expect_equal(length(kmedians(X = A, num_clusters = n)), 2)

  expect_equal(nrow(kmedians(X = A, num_clusters = n)[[1]]), n)
  # number of medians equals to number of clusters

  expect_equal(lengths(kmedians(X = A, num_clusters = n)[2]), nrow(A))
  # number of labels equals to number of data points
})

test_that("test if the medians and clustering given toy data are correct", {

  expect_equal(match(kmedians(X = A, num_clusters = n)[[1]],1), 2)

  expect_equal(match(kmedians(X = A, num_clusters = n)[[1]],100), 2)

  expect_equal(length(unique(kmedians(X = A, num_clusters = n)[[2]])), 2)

  expect_equal(match(kmedians(X = A, num_clusters = n)[[2]],0), 3)

  expect_equal(match(kmedians(X = A, num_clusters = n)[[2]],1), 3)

})
