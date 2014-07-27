## Put comments here that give an overall description of what your
## functions do

#This function creates a "matrix" object that can cache its inverse using 
# <<- operator to assign a value in the parent environment. 
#This object has 4 functions: set, get, setinverse, get inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { # Set a Value for the object
    x <<- y
    m <<- NULL
  }
  get <- function() x # Get the value of the object
  setinverse <- function(inverse) m <<- inverse # Set a value for the object in the parent environment 
  getinverse <- function() m # Get the value of the object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#--------------------------------------------------------------------------
# computes the inverse of a "matrix" object created by makeCacheMatrix. 
# If the inverse has already been calculated, then use the cache.
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m) # Set the value in the current environmet 
  m
  ## Return a matrix that is the inverse of 'x'
}





