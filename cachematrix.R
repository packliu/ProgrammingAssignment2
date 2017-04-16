## This R file includes functions of makeCacheMatrix and cacheSolve 
## to output the inverse of a square matrix by caching

## The function makeCacheMatrix creates a matrix 
## return a list including four functions: 
## set: set the value of the matrix
## get: get the value of the matrix
## setinversemat: set the inverse of the matrix
## getinversemat: get the inverse of the matrix

makeCacheMatrix <- function (x = matrix ()) {
  
  im <- NULL
  set <-function(y) {
    
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinversemat <- function(inversemat) im <<- inversemat
  getinversemat <- function() im
  list (set=set, get=get, 
        setinversemat=setinversemat, getinversemat=getinversemat)
}

## The function cacheSolve computes the inverse of the matrix created by
## the function makeCacheMatrix. The cached inverse is read if the inverse of 
## the matrix has been calculated, otherwise the inverse is computed and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  im <- x$getinversemat()
  if(!is.null(im)) {
    
    message ("getting cached data")
    return (im)
  }
  data <- x$get()
  im <- solve (data, ...)
  x$setinversemat(im)
  im
}

