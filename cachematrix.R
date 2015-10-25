## Put comments here that give an overall description of what your
## functions do

## This function make the Cache of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInvSqrtMatrix <- function(solve) {
        m <<- solve
    }
    getInvSqrtMatrix <- function() m
    
    list(set = set, 
         get = get, 
         setInvSqrtMatrix = setInvSqrtMatrix,
         getInvSqrtMatrix = getInvSqrtMatrix)
    
}

## This function solve the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getInvSqrtMatrix()
    if (!is.null(x)){
        message("Getting cached data...")
        return (m)
    }
    ## Calculate the inverse squared matrix
    data <- x$getMatrix()
    m <- solve(data)
    x$setInvSqrtMatrix(m)
    return (m)
}
