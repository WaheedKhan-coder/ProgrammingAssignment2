## The following functions are used to cache the inverse of a matrix.
## This avoids recomputing the inverse every time, improving performance
## when the inverse needs to be retrieved multiple times.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize the inverse as NULL
        
        # Function to set the matrix and reset cached inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Function to get the matrix
        get <- function() x
        
        # Function to set the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        # Function to get the cached inverse
        getinverse <- function() inv
        
        # Return a list of the above functions
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then the cached inverse is retrieved.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        # If inverse is already cached, return it
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
        # If not cached, compute the inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        
        # Cache the inverse
        x$setinverse(inv)
        
        inv  # Return the computed inverse
}

