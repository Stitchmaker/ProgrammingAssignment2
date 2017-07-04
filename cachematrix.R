## Inverting a matrix can be a computational intensive operation.
## If the inverse will be used multiple times, it is advantageous
## to cache the solution and retrieve it later if the inverse
## has already been computed.  
##
## call makeCacheMatrix with a square matrix to initiate a list of
## functions.  Call cacheSolve to either create the invense matrix
## or return the inverse which has been cached from a previous
## call to cacheSolve

## makeCacheMatrix creates a list of functions which are cached
## and accessed later with a call to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix(nrow=0,ncol=0)
  set <- function(y) {
    x <<- y
    m <<- matri(nrow=0,ncol=0)
  }
  get <- function() x
  setmatrix <- function(inv) m <<- inv
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve will retrieve a matrix from the cache if it is there
## or return the inverse of a matrix if it has not already been
## cached

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if((ncol(m)!=0) & (nrow(m)!=0)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}
