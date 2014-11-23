## These set of functions create a special matrix.  Then computes it inverse
## and caches the results. Then determines if the inverse has been calculated 
## before and the matrix has not changed.  If these are true then the cached
## results are returned.

## Create the matrix, calulate the inverse, store all in cache.

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvrs <- function(solve) m <<- solve
        getinvrs <- function() m
        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}


## Compare the inverse of the matrix and determine if the matrix has changed 

cacheSolve <- function(x, ...) {
        m <- x$getinvrs()
        if(!is.null(m)) {
                message("Cached results are returned")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvrs(m)
        m
}















