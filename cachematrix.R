## Created by jlrochet on 6-8-2016
## This code closely follows Prof. Peng's example on Coursera
## These two functions work together to solve for the inverse of a matrix

## makeCacheMatrix stores the matrix information in a list for caching
## I eliminated the "set" function from the example code because it doesn't seem
## to do anything in either function...

makeCacheMatrix <- function(x = matrix()) {
        mtx <- NULL
        get <- function() x
        setsolve <- function(solve) mtx <<- solve
        getsolve <- function() mtx
        list (get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve returns the cached inverse matrix or calculates 
## the inverse matrix absent a cached version

cacheSolve <- function(x, ...) {
        mtx <- x$getsolve()
        if(!is.null(mtx)) {
                message ("retrieving cached data...")
                return(mtx)
        }
        data <- x$get()
        mtx <- solve(data, ...)
        x$setsolve(mtx)
        mtx
}