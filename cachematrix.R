## Functions related to caching the inverse of a given matrix

## makeCacheMatrix
## Creates a "special" object of functions that can cache
## the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(invmtrx) matrix <<- invmtrx
  getInverse <- function() matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve
## Computes the inverse of the matrix using the "special"
## object from makeCacheMatrix.

cacheSolve <- function(x, ...) {
  matrix <- x$getInverse()
  ## Checks if cache is not empty
  if(!is.null(matrix)){ 
    message("getting cached data")
    return(matrix)
  }
  grid <- x$get()
  matrix <- solve(grid, ...)
  x$setInverse(matrix)
  matrix
}
