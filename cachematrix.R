## These functions create a special object that stores a matrix and
## cache's its inverse. If x is a square invertible matrix, than cacheSolve checks if 
## inverse has been calculated, then retrieve the inverse from the cache. 
## Otherwise, it calculates the inverse of the matrix and cache it via the setinv function.

## Creates a speacial "matrix" object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
      x <<- y
      m <<- NULL
    }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
