## Cache the Inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(b) {
    x <<- b
    inversed <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inversed <<- inverse
  getInverse <- function() inversed
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversed <- x$getInverse()
  if (!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  matrixed <- x$get()
  inversed <- solve(matrixed, ...)
  x$setInverse(inversed)
  inversed
}
