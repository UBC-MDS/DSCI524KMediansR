#' kmedians_summary tests
#'
#' Testing includes:
#'  checks the datatype of the output
#'  check if the number of clusters is correct
#'  check if the labels are correct
#'  check if the reported Median Coordinates are correct
#'  check if the numbers of data points in each cluster are correct
#'  check if the average distance within each cluster is correct
#'  check if the minimum distance within each cluster is correct
#'  check if the maximum distance within each cluster is correct


context("testing summary")

# toy data for calculation
A <- matrix(
  c(1,1,1,2,2,1,100,100,101,100,100,101),
  nrow = 6,
  ncol = 2,
  byrow = TRUE)

m <- matrix(
  c(1,1,100,100),
  nrow = 2,
  ncol = 2,
  byrow = TRUE)

l <- c(0,0,0,1,1,1)

test_that("test if output is a dataframe given toy data", {

  expect_equal(is.data.frame(summary(X = A, medians = m, labels = l)), TRUE)

})

test_that("test if number of clusters is correct", {

  expect_equal(nrow(summary(X = A, medians = m, labels = l)), 2)

})

test_that("test if the labels are correct", {

  expect_equal(summary(X = A, medians = m, labels = l)[1,1],0)

  expect_equal(summary(X = A, medians = m, labels = l)[2,1],1)

})

test_that("test if the reported Median Coordinates are correct", {

  expect_equal(summary(X = A, medians = m, labels = l)[1,2],c("1,1"))

  expect_equal(summary(X = A, medians = m, labels = l)[2,2],c("100,100"))

})


test_that("test if the numbers of data points in each cluster are correct", {

  expect_equal(summary(X = A, medians = m, labels = l)[1,3],3)

  expect_equal(summary(X = A, medians = m, labels = l)[2,3],3)

})


test_that("test if the average distance within each cluster is correct", {

  expect_equal(summary(X = A, medians = m, labels = l)[1,4],2/3)

  expect_equal(summary(X = A, medians = m, labels = l)[2,4],2/3)

})

test_that("test if the minimum distance within each cluster is correct", {

  expect_equal(summary(X = A, medians = m, labels = l)[1,5], 0)

  expect_equal(summary(X = A, medians = m, labels = l)[2,5], 0)

})


test_that("test if the maximum distance within each cluster is correct", {

  expect_equal(summary(X = A, medians = m, labels = l)[1,6], 1)

  expect_equal(summary(X = A, medians = m, labels = l)[2,6], 1)

})


