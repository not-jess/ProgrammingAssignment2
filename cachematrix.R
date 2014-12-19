## Calculating inverse matrices can be slow.  To save time, this pair of
## functions creates a list to store the matrix, it's inverse and methods
## to get and set the matrix, calculate it's inverse, and retrieve the
## cached inverse.  'makeCacheMatrix' should be called to create the cached
## matrix.  'cacheSolve' can then be called to get the inverse.

## This function creates a matrix-like object capable of storing it's own 
## inverse, so that we can save time by only calculating the inverse of the
## matrix once.
## Returns a list containing methods to get and set the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a cacheMatrix as created by the above function,
## 'makeCacheMatrix'.  It checks to see if the matrix has already been
## solved.  If it has, it returns the cached value.  If not, it calculates
## the inverse, caches it, and returns the inverse.
## Returns a matrix -- the inverse of the matrix passed to 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse();
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
