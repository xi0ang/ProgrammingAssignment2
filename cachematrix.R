## Put comments here that give an overall description of what your
## functions do

##The overall function is to cache the inverse of a matrix, 
##so that when we need the matrix again, it can be looked up in the cache rather than recomputed

## Write a short comment describing this function
## The first function is to creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    intert <<- NULL
  }
  get <- function() x
  setinvert <- function(inverse) invert <<- inverse
  getinvert <- function() invert
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
  
}


## Write a short comment describing this function

## This function is to computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  invert <- x$getinvert()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinvert(invert)
  ## Return a matrix that is the inverse of 'x'
}


test<-matrix(c(1:4),2,2)
test.1 <- makeCacheMatrix(test)
test.2 <- cacheSolve(test.1)