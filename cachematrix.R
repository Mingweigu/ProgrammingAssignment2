## Caching the inverse of a matrix

## makeCacheMatrix takes a matrix as an argument
## and returns a special matrix, which is actually
## a list that contains four functions:
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse of the matrix
## (4) get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes as an argument a "special" matrix
## as returned by makeCacheMatrix.
## If the inverse has been calculated before for the matrix,
## then return it without calculating again.
## If not, then calculate it and save the result to the
## "special" matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
