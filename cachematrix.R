## These functions invert a matrix and cache it. If the inverted matrix is in cache, 
## it isn't calculated again, but read from cache.

## Returns the object on which the inversion is performed. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversion <- function(solve) m <<- solve
  getinversion <- function() m
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## Checks if the inversion has already been calculated and reads it; if not,
## makes the calculation. The argument "x" is not a matrix, but the result of 
## function "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinversion()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversion(m)
  m
}
