## These functions create a "matrix" pseudo-object (list-based)
## and provide a cached 'solve' proxy.

## This function initializes the list for the matrix "object".
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function is a proxy for matrix 'solve' function. Arguments are 
## identical to 'solve', except that a cacheMatrix "object" must be provided
## instead of a primitive matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
