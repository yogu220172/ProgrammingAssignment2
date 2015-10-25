## Lexical Scoping Assignment 
## This assignment demonstrates the use of caching effectively in
## inverting matrices. The invserse of a matrix defined as 
##   A^-1 = adj(A)/|A| where |A| != 0 
## Using superassignment operators the given matrix & its inverse are 
## made available in global environment



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Containing environment is the global environment
  # Superassignment operator is us
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the given matrix
# If the inverse of the matrix is already availble in the scope,
# in the global scope, in this asignment, the cached value is retrived
# If the inverse is not cached, then it is computed and stored

cachesolve <- function(x, ...) {
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



