# Caches the inverse of a matrix.

# Given matrix 'x', makeCacheMatrix returns a special matrix object in preparation for caching the inverse.
# The special object defines functions that set and get the data, and set and get the inverse.
makeCacheMatrix <- function(x = matrix()) {
        # Initialize 'i' and define function to set up the data.
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # Define function to get the data.
        get <- function() x
        # Define function to set and cache the inverse.
        setinverse <- function(inv) i <<- inv
        # Define function to get the inverse.
        getinverse <- function() i
        # Return a list of these functions.
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Given the special matrix object (a list of functions returned by makeCacheMatrix),
# cacheSolve computes and caches the inverse 'i'.
cacheSolve <- function(x, ...) {
        # Get the inverse 'i'.
        i <- x$getinverse()
        # Check to see if 'i' has already been cached.
        # If it has, then return 'i'.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # If 'i' has not already been cached, then compute it from the data.
        data <- x$get()
        i <- solve(data, ...)
        # Cache and return 'i', the inverse of 'x'
        x$setinverse(i)
        i
}
