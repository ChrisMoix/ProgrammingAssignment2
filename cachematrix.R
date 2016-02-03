## These functions create a matrix
## With internal to get the matrix, set the inverse, get the inverse
## and cache the inverse

######################################################
## makeCacheMatrix:
## Takes a matrix as an input
## Creates internal functions to
##    Get the matrix (return it to caller)
##    Set (and calculate) the inverse of the matrix
##    get the inverse of the matrix
######################################################

makeCacheMatrix <- function(x = matrix()) {
  ##  
  ##
  
  inv <- NULL
  set <- function(y) {
    ##
    ##
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

########################################################
## cacheSolve:
## Calculates and returns the inverse of a 
##  matrix if it has not been previously returned
## If the matrix has already been returned
##  returns the cached matrix, instead of recalculating

########################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  ## Check to see if inv is already calculated and cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Calculate the inverse if it has not been cached
  data <- x$get()
  inv <- solve(data, ...)  
  
  #sets the inverse(d) value in the cache
  x$setInverse(inv)
  
  #returns the inverse 
  return(inv)
}
