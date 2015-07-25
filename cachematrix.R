## These functions cache a matrix object that can be inverted. First a list
## of necessary functions are created and the matrix is cached, then the
## inverse is returned.

## The matrix gets cached to be inverted, functions are created to run the
## inversion.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function attempts to retrieve cached inverse of a matrix, if impossible
## it inverses the matrix passed to it and caches it

cacheSolve <- function(x, ...) {
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