## Caching the Inverse of a Matrix:
## Converting a matrix in inversion benefit to caching
## the inverse of a matrix rather than compute it repeatedly.
## Below functions are used to create a special object that
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    rev <- NULL
    set <- function(y) {
        x <<- y
        rev <<- NULL
        
    }
    get <- function() x
    setInverse <- setInverse <- function(Inverse) rev <<- Inverse
    getInverse <- function() rev
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## The cachesolve function calculates the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse has already been calculated and the 
## matrix has not changed, then it would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    rev <- x$getInverse()
    if(!is.null(rev)) {
        message("getting cached data")
        return(rev)
    }
    data <- x$get()
    rev <- solve(data, ...)
    x$setInverse(rev)
    rev
}