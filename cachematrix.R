#----------------------------------------------------------------
# Description:
#
# A pair of functions makeCacheMatrix() and cacheSolve() that
# cache the inverse of a matrix.
#
# The function makeCacheMatrix() creates an object that can
# cache a matrix and its inverse. The function returns a list
# of four functions which store a matrix, return the matrix,
# store the matrix inverse and return the matrix inverse.
#
# The function cacheSolve() computes and caches the inverse of
# a matrix. The inverse is computed once and can be retrieved 
# multiple times. The function stores the matrix and inverse
# in the object created by the function makeCacheMatrix().
#
#*****************************************************************
# Example:
#
# Create an invertible n X n matrix
# > mat <- matrix(c(1,2,3,4),2,2)
# > mat
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
#
# Create a cache-matrix object
# > cm <- makeCacheMatrix(mat)
#
# Compute, cache and return the matrix 
# > inv <- cacheSolve(cm)
# > inv
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# Return the cached matrix inverse
# > inv2 <- cacheSolve(cm)
# getting cached inverse
# > inv2
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
#-----------------------------------------------------------------

#-----------------------------------------------------------------
#
# Function: makeCacheMatrix
#
# Create a function object that caches a matrix and its inverse
#
# Usage: makeCacheMatrix(m)
#
# Argument: m          an invertible matrix, optional
#
# Returns: a list L of functions
#
#   L$set(m)           store the matrix.  m - an invertible matrix
#   L$get()            get the matrix.    returns the matrix
#   L$setinverse(i)    store the inverse. i - the matrix inverse
#   L$getinverse()     get the inverse.   returns the inverse
#
#-----------------------------------------------------------------

makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(m) {
    mat <<- m
    inverse <<- NULL
  }
  get <- function() mat
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#-----------------------------------------------------------------
#
# Function: cacheSolve
#
# If the matrix inverse is not in cache, compute the inverse and
# cache it.  Return the cached matrix inverse.
#
# Usage: cacheSolve(cm)
#
# Argument: cm         the object returned from makeCacheMatrix
#
# Returns: the inverse of a matrix
#
#-----------------------------------------------------------------

cacheSolve <- function(cachemat, ...) {
  inverse <- cachemat$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- cachemat$get()
  inverse <- solve(data, ...)
  cachemat$setinverse(inverse)
  inverse
}