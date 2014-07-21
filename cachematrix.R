## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix creates a matrix to be cached. It is also 
## a placeholder for the inversed matrix, once this is created.

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(y) {
    x <<- y
    cm <<- NULL
  }
  get <- function() x
  setMatrixCached <- function(inversedMatrix) cm <<- inversedMatrix
  getMatrixCached <- function() cm
  list(set = set, get = get,
       setMatrixCached = setMatrixCached,
       getMatrixCached = getMatrixCached)
}


## Write a short comment describing this function
## The cacheSolve function first get the cached inversed matrix. 
## If this is null, it gets the matrix and inverses it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cm <- x$getMatrixCached()
  if(!is.null(cm)) {
    message("getting cached data")
    return(cm)
  }
  data <- x$get()
  cm <- solve(data, ...)
  x$setMatrixCached(cm)
  cm
}
