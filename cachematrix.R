## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Create a container object which houses a matrix.
# The container can hold the matrix inverse as well.
# The container provides setter and getter functions
# for reading and writing the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize the inverse of the matrix
    inv <- NULL
    
    # Setter for new x
    # Generates a new environment with x
    # (no inverse yet because it is not known)
    set <- function(y) {
        x <<- y
        inv <<- NULL }
        
    # Getter - call to access matrix
    get <- function() x
    
    # Setter for inverse - called to overwrite NULL
    setinv <- function(inverse) inv <<- inverse
    
    # Getter - call to access matrix inverse
    getinv <- function() inv
    
    # Result is a list of the setters and getters
    #     for a matrix and its inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# Wrapper function for solve().
# Uses containers from makeCacheMatrix to cache
# previously computed inverses to improve runtime
# with repeated use.
cacheSolve <- function(x, ...) {
    # See if we've already computed an inverse for this matrix
    inv <- x$getinv()
    
    # Did we find an existing inverse?
    if(!is.null(inv)) {
        # Yes - so we're done!
        message("retreiving cached inverse")
        return(inv)
    }
    
    # No inverse found, so we have to compute it
    # Grab the matrix...
    data <- x$get()
    
    # Compute its inverse
    inv <- solve(data)
    
    # Remember it for later
    x$setinv(inv)
    
    # Result: The inverse of the matrix
    inv  
        
    }
