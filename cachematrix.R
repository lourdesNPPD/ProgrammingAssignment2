## Functions that will cache the inverse of a matrix so the inverse of the same matrix does not have
## to be recalculated repeatedly.

## Creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- matrix()
    set <- function(y){
      x <<- y
      invMatrix <<- NULL
    }
    get <- function() x
    setInv <- function(matrix) invMatrix <<- matrix
    getInv <- function() invMatrix
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Computes the inverse of the matrix object returned by makeCacheMatrix. If the inverse has already been calculated
## and the matrix has not changed, the function retrieves the cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInv()
    if(!is.null(invMatrix)){
        message("Getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data)
    x$setInv(invMatrix)
    invMatrix
  
}
