## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  setm <- function(y) {
    x <<- y
    minv <<- NULL
  }
  getm <- function() x
  setminv <- function(solve) minv <<- solve
  getminv <- function() minv
  list(setm = setm, getm = getm,
       setminv = setminv,
       getminv = getminv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getminv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  datam <- x$getm()
  minv <- solve(datam, ...)
  x$setminv(minv)
  minv
}


