#----------------------------------------------------------------
# Description:
#
# A pair of functions makeCacheMatrix() and cacheSolve() that
# cache the inverse of an invertible matrix.
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
# Version 1.0   2015-10-24   initial release
#
#*****************************************************************
# Example:
#
# Create an n X n matrix
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
# > inv <- cacheSolve(cm)
# getting cached inverse
# > inv
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
#-----------------------------------------------------------------

#-----------------------------------------------------------------
#
# Function: makeCacheMatrix
#
# Create an object that caches a matrix and its inverse
#
# Usage: makeCacheMatrix(matrix)
#
# Argument: matrix          an invertible matrix, optional
#
# Returns: a list L of functions
#
#   L$setMatrix(matrix)     store the matrix. Must be invertible.
#   L$getMatrix()           get the matrix.
#   L$setInverse(inverse)   store the matrix inverse.
#   L$getInverse()          get the matrix inverse.
#
#-----------------------------------------------------------------

makeCacheMatrix <- function(myMatrix = matrix()) {
  # initialize matrix inverse
  myInverse <- NULL
  
  # function resets matrix and matrix inverse
  # allows cacheMatrix object to be reused with a new matrix
  funSetMatrix <- function(m) {
    myMatrix <<- m
    myInverse <<- NULL
  }
  
  # function returns matrix
  funGetMatrix <- function() myMatrix
  
  # function stores matrix inverse
  funSetInverse <- function(i) myInverse <<- i
  
  # function returns matrix inverse
  funGetInverse <- function() myInverse
  
  # return list of functions
  list(setMatrix  = funSetMatrix,
       getMatrix  = funGetMatrix,
       setInverse = funSetInverse,
       getInverse = funGetInverse)
}

#-----------------------------------------------------------------
#
# Function: cacheSolve
#
# If the matrix inverse is not in cache, compute the inverse,
# cache it.  Return the cached matrix inverse.
#
# Usage: cacheSolve(cacheMatrix)
#
# Argument: cacheMatrix   the object returned from makeCacheMatrix
#
# Returns: the inverse of a matrix
#
#-----------------------------------------------------------------

cacheSolve <- function(cacheMatrix, ...) {
  # get matrix inverse stored in cacheMatrix object
  inverse <- cacheMatrix$getInverse()
  
  # if the inverse has already been computed and stored  
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  # get matrix stored in cacheMatrix object
  m <- cacheMatrix$getMatrix()
  
  # compute inverse of matrix
  inverse <- solve(m, ...)
  
  # store inverse in cacheMatrix object
  cacheMatrix$setInverse(inverse)
  
  # return matrix inverse
  inverse
}