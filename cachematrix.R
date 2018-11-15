## makeCacheMatrix - defines a set of functions around a matrix and its inverse. 
## -- It is used when establishing a matrix and will cache the results after cacheSolve is called

makeCacheMatrix <- function(x = matrix()) {
  # initialize to the empty matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - returns the inverse of a matrix. The first time it is called with a particular
##     matrix it will calculate it. On subsequent calls to do the same action on the matrix it
##     take it from the cached value

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mtrx <- x$get()
  inv <- ginv(mtrx)
  x$setinverse(inv)
  inv
}

