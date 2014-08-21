## Methods for caching matrix inversion
#
## Usage:
##
## > mtx <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## > cacheSolve(mtx)
##
## When called a second time, with no changes to the
## matrix, a cached version of the inverse will be returned.

## Wraps a matrix with functions for caching its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Pass in a wrapped matrix returned by makeCacheMatrix and
## method will check for a cached value of the inverse or
## calculate the inverse and cache for future calls.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}