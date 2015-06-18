## The two functions berlow are used to create a special object that stores a square matrix and 
## cache its inverse.

## The function makeCacheMatrix creates a special square "matrix" object
## that can cache its inverse. The special matrix contains functions that:
##### set the matrix
##### get the matrix
##### set the inverse of the matrix
##### get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The cacheSolve function returns a matrix that is the inverse of x. First cacheSolve  
## checks if the inverse of the matrix has already been calculated using the output of makeCacheMatrix. 
## If this is the case, cacheSolve gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets it in the cache via 
## the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix_data <- x$get()
        inv <- solve(matrix_data, ...)
        x$setinv(inv)
        inv
}
