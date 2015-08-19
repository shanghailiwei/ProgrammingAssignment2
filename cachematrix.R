## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
