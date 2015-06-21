## Functions related to caching the inverse of a given matrix

## makeCacheMatrix
## Creates a "special" object of functions that can cache
## the inverse of a matrix
## Arguments: x - an invertible matrix
## Returns: a list of getter/setter functions for storing
##      and retrieving the original and inverted matrices

makeCacheMatrix <- function(x = matrix()) {
  cachematrix <- NULL #cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(invmtrx) cachematrix <<- invmtrx
  getInverse <- function() cachematrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve
## Computes the inverse of the matrix using the "special"
## object from makeCacheMatrix.
## Arguments: x - the result from makeCacheMatrix
## Returns: invmatrix - the inverse of the original matrix

cacheSolve <- function(x, ...) {
  invmatrix <- x$getInverse()
  ## Checks if cache is not empty
  if(!is.null(invmatrix)){ 
    message("getting cached data")
    return(invmatrix) #returns the contents of the cache
  }
  ##Cache is empty, solve for the inverse
  grid <- x$get() #gets the original matrix
  invmatrix <- solve(grid, ...) #matrix is inverted
  x$setInverse(invmatrix) #the inverted matrix is stored in the cache
  invmatrix
}
