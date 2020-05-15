## Put comments here that give an overall description of what your
## functions do

## These are an R functions that are able to cache potentially time-consuming computations in matrix inversion trough caching the inverse of a matrix rather than computing it repeatedly  

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m=NULL
  set=function(y){
    x <<- y
    m <<- NULL
  }
  get = function() x
  setinv = function(inv) m <<- inv
  getinv = function() m
  
  list(set= set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
              ## Return a matrix that is the inverse of 'x'
  m = x$getinv()
  if (!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data = x$get()
  m= solve(data, ...)
  x$setinv(m)
  m
}
