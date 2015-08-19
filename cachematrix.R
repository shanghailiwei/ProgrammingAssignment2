# A pair of functions that cache the Inverse of a matrix

# Creates a matrix object that can cache its Inverse
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the Inverse
# 4. Get the value of the Inverse
makeCacheMatrix <- function(x = matrix()) {
        # Initialize the Inverse matrix
        m <- NULL
        
        # Set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # Get the matrix
        get <- function() 
                x
        
        # Set the Inverse of the matrix
        setInv <- function(Inverse) 
                m <<- Inverse
        # Get the Inverse of the matrix
        getInv <- function()
                m
        
        # Return a list of the methods
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

# Compute the Inverse of the special matrix returned by "makeCacheMatrix" above
# If the Inverse has already been calculated (and the matrix has not
# changed), then the "cacheSolve" should retrieve the Inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInv()
        
        # If the Inverse matrix is already set, return it
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # The Inverse is not yet calculated, so we calculate it
        data <- x$get()
        m <- solve(data,...)
        
        # set the Inverse to the matrix
        x$setInv(m)
        
        # Return the matrix
        m
}