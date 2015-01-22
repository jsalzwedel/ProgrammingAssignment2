## These functions are for storing and manipulating square matrices and their 
## inverses.
## One function makes a special matrix object (actually a list of matrix
## manipulation functions) that is able to store the cache of its inverse.
## The other function is able to calculate or retrieve the cached inverse of
## a matrix.

## This function returns a list of functions that can be used to manipulate
## a square matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## The 'set' function sets the value of the matrix and clears any information
    ## about the matrix's inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## The 'get' function returns the matrix
    get <- function() x
    ## Function to cache the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    ## Function to retrieve a cached inverse of the matrix
    getInverse <- function() inv
    ## Return the list of matrix manipulation functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This functions returns the inverse of the matrix in a makeCacheMatrix list
## If the inverse has already been found, cacheSolve retrieves the inverse'
## from the the special matrix object. Otherwise, cacheSolve finds the inverse,
## stores the inverse in the matrix object, and returns the inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## 'x" must be an invertible square matrix.
    
    ## Check if the inverse has already been found
    inv <- x$getInverse()
    if(!is.null(inv)) {
        ## if the inverse has been found, return it
        message("getting cached data")
        return(inv)
    }
    ## If the inverse hasn't been found, solve for it.
    ## First retrieve the matrix
    matrix <- x$get()
    ## Solve for the inverse
    inv <- solve(matrix)
    ## Set the inverse in the makeCacheMatrix object so that it can be
    ## used later
    x$setInverse(inv)
    ## Return the inverse
    inv
}
