
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    setMatrix <- function(y) {
        mat <<- y
        inv_m <<- NULL
    }
    getMatrix <- function() {
        mat
    }
    setMatrixinverse <- function(matinv) {
        inv_m <<- matinv
 
    }
    getMatrixinverse <- function() {
        inv_m
    }
    list(setMatrix=setMatrix, getMatrix=getMatrix, setMatrixinverse=setMatrixinverse, getMatrixinverse=getMatrixinverse)
}

# The following function returns the inverse of the matrix. 
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv_m <- x$getMatrixinverse()
    if(!is.null(inv_m)) {
        message("Getting cached data.")
        return(inv_m)
    }
    data <- x$getMatrix()
    inv_m <- solve(data)
    x$setMatrixinv_merse(inv_m)
    inv_m
}

