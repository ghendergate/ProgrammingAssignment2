## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Object that stores a matrix and can cache its inverse
        m <- NULL
        # Set the input matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # Get the input matrix
        get <- function() x
        # Calculate the inverse matrix
        setinverse <- function(x) {m <<- solve(x)}
        # Get the cached inverse matrix
        getinverse <- function() m
        list(set = set, get = get,
             getinverse = getinverse,
             setinverse = setinverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Try to read a cached inverse matrix
        ## and calculate if not cached or if
        ## input matrix changes
        m <- x$getinverse()
        # If inverse matrix exists 
        if (!is.null(m)) {
                # If identity matrix has unity diag
                # i.e. check if matrix has changed
                # since inverse was calculated
                #if (all(diag(x$get() %*% m) == 1)){
                #        message("getting cached data")
                #        return(m)
                #}
        }
        # Calculated inverse matrix if not already cached 
        # or if input matrix changes
        data <- x$get()
        x$setinverse(data)
        # Return inverse matrix
        x$getinverse()
}
