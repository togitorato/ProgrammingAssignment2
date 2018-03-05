## makeCacheMatrix creates a matrix object, and cacheSolve finds the inverse
## of the special matrix object. It also gets the inverse value from the cache,
## if it is calculated before, and if there has been no change in the matrix

## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special matrix. If the matrix is
## still the same and the inverse is already calculated, cacheSolve returns
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()

    if (!is.null(m)) {
        message("Getting cached data :)")
        return(m)
    }

    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
