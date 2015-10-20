
## author: burtr (after a template of rdpeng)
## created: 20 oct 2015
## last-update: 20 oct 2015

## These functions implement a matrix-with-cached-inverse
## object.

## makeCacheMatrix takes an optional operation and
## creates a matrix-with-cached-inverse object. 
## getter's and setter's are provided for both the 
## matrix and the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        cached_inv <- NULL
        set <- function(y) {
                x <<- y
                cached_inv <<- NULL
        }
        get <- function() x
        setinv <- function(m) cached_inv <<- m
        getinv <- function() cached_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve retrieves the cached inverse from a
## matrix with catched inverse object; and will calculate
## and cache the inverse of the marix, if not already
## cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}

