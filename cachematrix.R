## Put comments here that give an overall description of what your
## functions do

#This function creates a "matrix" object that can cache its inverse using 
# <<- operator to assign a value in the parent environment. 
#This object has 4 functions: set, get, setinverse, get inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# computes the inverse of a "matrix" object created by makeCacheMatrix. 
# If the inverse has already been calculated, then use the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}





