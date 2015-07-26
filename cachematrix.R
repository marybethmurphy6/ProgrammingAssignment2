## A pair of functions that cache inverse of a matrix:

## makeCacheMatrix() is a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## cacheSolve() computes inverse of makeCacheMatrix()
## retrieve inverse from cache if already calculated
## otherwise compute inverse and cache the result

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if (!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m         ## Return a matrix that is the inverse of 'x'
}
