## Matrix operations can be costly and can benefit greatly from caching these operations.
## This file provides a "special" matrix representation that support caching operations. 
## Caching the matrix inversion, i.e. solve, operation has been implemented, adding 
## additional "cached" operations is quite easy.


## This function creates a "special" matrix representation that support caching the matrix 
## inversion, i.e. solve, operation.
##
## @param x   A typical R matrix
## @returns   A "special" matrix representation, calling x$get() returns the original matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function performs the matrix inversion operation on a "special" matrix as constructed
## by a call to the makeCacheMatrix-function. Matrix inversion itself is only performed once,
## subsequent calls to cacheSolve with the same input matrix will return the cached result.
##
## @param x   A "special" matrix (built by the makeCacheMatrix-function).
## @returns   The inverse of the matrix that was provided.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
