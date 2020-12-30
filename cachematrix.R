## This R file has two functions "makeCacheMatrix" & "cacheSolve".
# The intention of these function is to get a square matrix as an input
# Compute the inverse of the Matrix and Cache it


#The below function creates a special “matrix” object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  #initalize the inverse to NULL
  inv <- NULL
  
  # set function for Matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get function for Matrix
  get <- function() x
  
  #set function for Inverse
  setinv <- function(inverse) inv <<- inverse
  
  #get function for Inverse
  getinv <- function() inv
  
  #returns a list with both get and set values
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


# The below function computes the inverse of the special “matrix” returned by makeCacheMatrix function above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # get the stored inverse matrix
  inv <- x$getinv()
  
  #if the Inverse is available return a message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #else calculate and cache the inverse of the matrix (using functions from makeCacheMatrix created above)
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  #return the Inverse of the Matrix
  return(inv)
}