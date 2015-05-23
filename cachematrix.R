## This file contains two functions that facilitate the creation of a matrix
## and compute the inverse of the matrix by using cache mechanism to avoid
## costly computation

## 'makeCacheMatrix' function will create a special 'matrix' object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize a variable to store the inverse of matrix 'x'
  inv <- NULL
  
  # reassign existing matrix object to a new value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # return current matrix
  get <- function(y) {
    x
  }
  
  # set the inverse of a matrix
  setInverse <- function(i) {
    inv <<- i
  }
  
  # get the inverse matrix
  getInverse <- function() {
    inv
  }
  
  # create a placeholder of all internal functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## 'cacheSolve' fuction computes the inverse of the special 'matrix'
## returned by 'makeCacheMatrix' function above.
## If the inverse has already been computed (and the matrix has not changed), 
## then this function will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Attempt to retrieve the inverse of matrix 'x'
  inv <- x$getInverse()
  
  # If the inverse of matrix 'x' is known, return it from cache storage
  if(!is.null(inv)) {
    message("Retrieving inverse matrix from cache...")
    return(inv)
  }
  
  # otherwise, compute the inverse of matrix 'x' and cache it
  data <- x$get()
  inv <- solve(data)
  
  # set the computed inverse of matrix 'x'
  x$setInverse(inv)
  inv
}
