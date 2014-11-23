## These set of functions get a matrix.  Then computes its inverse
## and caches the results. Then determines if the inverse has been calculated
## before.  If true then the cached results are returned.

## For function "makeCacheMatrix", input the matrix "x", then create the
## functions called by "cacheSolve".

makeCacheMatrix <- function(x = numeric()) {
        
        ## Use "i" as the inverse, set it to NULL each time "makeCacheMatrx"
        ## is called
        i <- NULL
        
        
        ## Set the stored inverse of the matrix to NULL and new value for "x"
        set <- function(new_x) {
                x <<- new_x
                i <<- NULL
        }
        
        ## Returns the value of the matrix the first time running "cacheSolve"
        get <- function() x
        
        ## Sets the inverse of the matrix when "cacheSolve" is first run
        setinvrs <- function(solve) i <<- solve
        
        ## This returns the cache value to "cacheSolve" after the first run
        getinvrs <- function() i
        
        ## Creast a list for the functions so they can be called
        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}

## Determine if the results are cached.  IF they are return a message and the
## cache results.  If not then calulate the inverse of the mateix and cache it.

cacheSolve <- function(x, ...) {
        
        ## Get the value of the "x" and assign it to "i".
        i <- x$getinvrs()
        
        ## If inverse already calculated then send this message, return the
        ## cache results and end the function.
        if(!is.null(i)) {
                message("Cached results are returned")
                return(i)
        }
        
        ## If x$getinvrs is NULL then get the matrix, calculate the inverse
        ## and cache it.
        data <- x$get()
        i <- solve(data, ...)
        x$setinvrs(i)
        i
}
