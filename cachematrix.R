## The following functions calculate the inverse of a matrix and cache 
## the result. If the resultant inverse is already cached, the calculation
## is skipped and the cached result returned.

## The makeCacheSolve function cache's and retrieves the inverse result.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <-function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             getinv = getinv,
             setinv = setinv)
}

## The cacheSolve function calculates the inverse of the matrix, unless 
## it's already been calculated, in which case the cached result is
## retrieved.
cacheSolve <- function(x, ...) {
        ## Return a matrix 'm' that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached result")
                return(m)
        }
        result <- x$get()
        m <- solve(result)
        x$setinv(m)
        m
}
