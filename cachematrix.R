## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.  
## The makeCacheMatrix and cacheSolve function pair provide a matrix inversion 
## result caching solution. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## s holds the cached result of solve(x)
    s <- NULL
    ## set the matrix and clear the cache
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    ## return the matrix 
    get <- function() x
    ## set the solved matrix 
    setsolve <- function(solve) s <<- solve
    ## return the solved matrix
    getsolve <- function() s
    ## the list methods for the function
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by the 
## makeCacheMatrix function. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    ## use the cached value if set
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    ## get the original matrix
    data <- x$get()
    ## solve the matrix 
    s <- solve(data, ...)
    ## cache the solved matrix and return it
    x$setsolve(s)
    s
}
