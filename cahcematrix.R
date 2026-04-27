# Programming Assignment 2: Caching the Inverse of a Matrix
# The idea is to avoid recalculating the inverse every time if it hasn't changed

makeCacheMatrix <- function(x = matrix()) {
  
  # m will hold the cached inverse, starts as NULL (not yet calculated)
  m <- NULL
  
  # set() lets you update the matrix and resets the cache
  set <- function(y) {
    x <<- y
    m <<- NULL  # important: clear old cached inverse when matrix changes
  }
  
  # get() simply returns the current matrix
  get <- function() x
  
  # setinverse() stores the computed inverse in the cache
  setinverse <- function(inverse) m <<- inverse
  
  # getinverse() retrieves the cached inverse (or NULL if not cached yet)
  getinverse <- function() m
  
  # return all four functions as a list so they can be accessed from outside
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  
  # first check if we already have a cached inverse
  m <- x$getinverse()
  
  if (!is.null(m)) {
    # if yes, just return it directly without recalculating
    message("getting cached data")
    return(m)
  }
  
  # if no cached value exists, get the matrix and calculate the inverse
  data <- x$get()
  m <- solve(data, ...)
  
  # store the result in the cache for next time
  x$setinverse(m)
  
  m
}