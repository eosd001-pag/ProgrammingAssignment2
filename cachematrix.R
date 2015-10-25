## This two function, store the any matrix into cache
## Recall the inverse squareroot of the matrix if it's been calculated
## before, otherwise will create the Inverse Sqrt Matrix and 
## store the result into cache

## This function make the Cache of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    ## Set the inverse square matrix
    setInvSqrtMatrix <- function(solve) {
        m <<- solve
    }
    # This gets the inverse square matrix from the cache
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
