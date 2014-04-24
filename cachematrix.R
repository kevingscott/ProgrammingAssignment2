## These function implement a solution to cache the inverse of a matrix
## callers to these functions should first call makeCacheMatrix to get the
## list of functions, and then cacheSolve to get the inverse of the matrix
## additional calls to the cacheSolve function will return the cached matrix

## this function returns a list of functions that are used by cacheSolve to 
##store a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve should be called with an x that is produced by a call to makeCacheMatrix
## the first cal when produce the inverse, and then additional calls will return
## the cached inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
