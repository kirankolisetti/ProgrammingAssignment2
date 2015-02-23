
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_m <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv_m <- x$getinverse()
  if(!is.null(inv_m)) {
    message("getting cached data.")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data)
  x$setinverse(inv_m)
  inv_m
}
