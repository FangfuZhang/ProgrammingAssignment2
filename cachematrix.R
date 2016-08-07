## The purpose of the two functions in this file is to cache potentially time-consuming 
## matrix inversion computation, rather than compute it repeatedly. To do this, two functions 
## are used, one makeCacheMatrix, and the other cacheSolve.

## This function creates a special "matrix" object that can cache its inverse. 
## The first function creates and returns a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by the function 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv        ## Return a matrix that is the inverse of 'x'
}


