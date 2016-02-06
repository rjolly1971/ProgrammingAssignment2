#Matrix inversion is usually a costly computation and there may be some 
#benefit to caching the inverse of a matrix rather than compute it 
#repeatedly.
#The code below are 2 functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inversion <- NULL 
  set <- function(y) {
    x <<- y
    inversion <<- NULL
  }
  get <- function() x 
  setinversion <- function(inverse) inversion <<- inverse
  getinversion <- function() inversion
  list(set = set, 
        get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}

## This function below computes the inverse of makeVector. 
## If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inversion <- x$getinversion()
  if(!is.null(inversion)) {
    message("getting cached data")
    return(inversion)
  }
  data <- x$get()
  inversion <- solve(data, ...)
  x$setinversion(inversion)
  inversion
}