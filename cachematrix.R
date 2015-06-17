## Functions makeCacheMatrix and cacheSolve are used to compute the inverse of
## a square, invertible matrix. The result is stored in a matrix and cached.
## If a request to inverse the same matrix is made again, the cached version
## is used.

## makeChacheMatrix is a constructor function that creates a matrix containing
## another matrix and several helper functions.
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


## The cacheSolve function takes a matrix, inverts the matrix and checks if the
## inverted matrix exists in the cache. If so it returns the cached version,
## otherwise the inverted version is returned and cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
