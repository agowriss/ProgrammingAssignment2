## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special object that can cache Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, 
       getinv = getinv, 
       setinv = setinv)
  
  
}


## Write a short comment describing this function
# This function returns an Inverse of a Matrix from Cache if available else calculates the inverse and returns the Inverse if the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}