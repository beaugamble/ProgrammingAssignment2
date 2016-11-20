## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inverse_m <- NULL
      set <- function(y) {
            x <<- y
            inverse_m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inverse_m <<- solve
      getinverse <- function() inverse_m
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse_m <- x$getinverse()
      if(!is.null(inverse_m)) {
            message("getting cached data")
            return(inverse_m)
      }
      data <- x$get()
      inverse_m <- solve(data, ...)
      x$setinverse(inverse_m)
      inverse_m
}
