## This pair of functions work alongside each other to cache an invertible
## square matrix, find the inverse of the matrix, then cache the inverse
## of the matrix.

## Creates cache for the inverse of a matrix; returns list of functions
## specific to setting and getting the matrix and the inverse of the
## matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {
        
        x <<- y
        i <<- NULL
        
    }
    
    get <- function() x
    
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
    
}


## Checks if 'x' is the inverse of the matrix from makeCacheMatrix
## function. If not, solves for the inverse of the matrix. In both cases,
## caches the inverse through makeCacheMatrix.

cacheSolve <- function(x, ...) {
    
    i <- x$getinv()
    
    if(!is.null(i)) {
        
        message("Getting cached data...")
        return(i)
        
    }
    
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
    
}
