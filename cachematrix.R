## These two functions are used to optimize multiple inversitions
## for the same matrix by caching inversion result.
## First "cached" matrix has to be created based on a matrix
## with makeCacheMatrix() and then cacheSolve inverts it and caches
## the inversion for the first time it is called.

## makeCacheMatrix is a helper object, which stores a matrix
## and its inversion and provides access methods for both

makeCacheMatrix <- function(x = matrix()) {
  ## returns a list of get/set methods to access a matrix
  ## and its inversion
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is similar to solve function - it inverts a matrix
## but it also caches the inversion to optimize future calls to find
## another inversion for the same matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if( !is.null(inv) ) {
    ## This matrix has been already inversed - retrieving from cache
    message("Returning cached inverted matrix...")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  ## Storing result in cache for future use
  x$setinverse(inv)
  inv
}
