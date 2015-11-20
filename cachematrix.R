
## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes in a square matrix of any size as a parameter
## and returns a list of functions. The functions are get'ters and
## set'ters of cached variables as well as the matrix inverse function
## solve. See ?solve for more information.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)    
}


## Write a short comment describing this function
## cacheSolve takes in the function list produced by
## mackeCacheMatrix and returns the inverse of the matrix
## that was the input parameter of makeCacheMatrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
