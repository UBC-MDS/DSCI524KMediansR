#' distance tests
#'
#' Testing includes:
#'  checks errors of the missing input
#'  checks errors of the datatype of the input
#'  checks the datatype of the output
#'  checks the size of the output
#'  correctly calculate the manhattan distance of the toy example

context("testing manhattan distance")

# toy data for calculation
A <- matrix(
  c(1,2,3,4,5,6),
  nrow = 3,
  ncol = 2,
  byrow = TRUE)
m <- matrix(
  c(1,1,2,2),
  nrow = 2,
  ncol = 2,
  byrow = TRUE)

test_that("missing input errors", {

  expect_error(distance(X = NULL, medians = m),

               'non-numeric matrix extent')

  expect_error(distance(X = A, medians = NULL),

               'non-numeric matrix extent')

})

test_that("datatype errors", {

  expect_error(distance(X = "abcd", medians = m),

               'non-numeric matrix extent')

  expect_error(distance(X = 100, medians = m),

               'non-numeric matrix extent')

  expect_error(distance(X = c(1,2), medians = m),

               'non-numeric matrix extent')

  expect_error(distance(X = list(c(1,2)), medians = m),

               'non-numeric matrix extent')

  expect_error(distance(X = A, medians = "abcd"),

               'non-numeric matrix extent')

  expect_error(distance(X = A, medians = 100),

               'non-numeric matrix extent')

  expect_error(distance(X = A, medians = c(1,2)),

               'non-numeric matrix extent')

  expect_error(distance(X = A, medians = list(c(1,2)),

               'non-numeric matrix extent'))

})

test_that("test if output is matrix given toy data", {

  expect_equal(is.matrix(distance(X = A, medians = m)), TRUE)

})

test_that("test size of output given toy data", {

  expect_equal(nrow(distance(X = A, medians = m)), nrow(A))

  expect_equal(ncol(distance(X = A, medians = m)), nrow(m))

})

test_that("test if the distance given toy data are right", {

  expect_equal(distance(X = A, medians = m)[,1], c(1,5,9))

  expect_equal(distance(X = A, medians = m)[,2], c(1,3,7))

})
