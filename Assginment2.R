makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invs <<- inverse
  getInverse <- function() invs
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invs <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  invs <- solve(mat, ...)
  x$setInverse(invs)
  invs
}





 