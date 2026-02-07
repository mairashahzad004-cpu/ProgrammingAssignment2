## Put comments here that give an overall description of what your
## functions do
## This file contains functions that create a special matrix object
## which can cache its inverse. If the inverse has already been computed, it is retrieved from the cache rather than recomputed.

## Write a short comment describing this function
## This file contains functions that create a special matrix object that can cache its inverse. If the inverse has already been computed, it is retrieved from the cache rather than recomputed.

makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
        set<- function (y) {
                x<<-y
                inv<<- NULL
}
        get<- function () x
        setinverse <- function(inverse) inv<<-inverse
        getinverse <- function() inv

list(
        set=set,
        get=get,
        setinverse=setinverse,
        getinverse= getinverse
)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" object.
## If the inverse has already been computed, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message (" getting cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
