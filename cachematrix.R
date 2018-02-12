## These are two functions that can cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ### Set value of matrix, reset value of inverse
        setmatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ### Get value of matrix
        getmatrix <- function() x
        
        ## Set value of inverse
        setinverse <- function(inverse) i <<- inverse
        
        ### Get value of inverse
        getinverse <- function() i
        
        ### Output list
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        ## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## Return a matrix that is the inverse of 'x'
        data <- x$getmatrix()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}