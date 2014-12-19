## Together, these functions create a special matrix, store 
## its inverse in the cache, and return its value by calculating 
## it from the cache the first time it is run and from the cache 
## each subsequent time it is run.


## Creates a matrix and stores its inverse in the cache

makeCachematrix <- function(x = matrix()) {
        s <- NULL
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Returns the inverse of the matrix created above by calculating
it or retrieving it from the cache (if it has been run previously)

cacheSolve <- function(x, ...) {
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
