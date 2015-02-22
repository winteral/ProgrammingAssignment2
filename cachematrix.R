## Put comments here that give an overall description of what your
## functions do

## Take a invertable matrix and provide functions to set and get the matrix
## and setinverse and getinverse the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL
  set <- function(y) {
    x <<- y
    cachedInv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) cachedInv <<- inv
  getinverse <- function() cachedInv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Take a special matrix object derived from makeCacheMatrix().
## Calculate the inverse of the given matrix object.
## Return cached value if possible

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
