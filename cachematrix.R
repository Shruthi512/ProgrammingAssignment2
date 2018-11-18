## This function computes the inverse of the special “matrix” returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
          x <<- y
          a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  a <- x$getinverse()
  if (!is.null(a)) {
          message("getting cached data")
          return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}
