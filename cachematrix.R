## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
