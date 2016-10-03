# COURSERA R Programming Assignment 02: Caching the Inverse of a Matrix 
# Author: LAM CHOON HO
# Date: OCTOBER 2016

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly 

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
    # initiate Inverse Value to NNULL
    invVal <- NULL
    
    # Give value to Cached Matrix & NULL to invVal
    set <- function(y){
        x <<- y
        invVal <<- NULL
    }
  
    # get, setinverseval, getinverseval for Cache Matrix
    get <- function() x
    setinverseval <- function(inverseval) invVal <<- inverseval
    getinverseval <- function() invVal
    
    # return list which contain all functions that are defined
    list(set = set, get = get,
         setinverseval = setinverseval,
         getinverseval = getinverseval)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...){
    # Retrive Cached Matrix value from cache
    matrixVal <- x$getinverseval()
    if(!is.null(matrixVal)){
        message("getting cached matrix value")
        return(matrixVal)
    }
    
    # if() function is ignored if the matrixVal is NULL
    # get natrixVal value
    data <- x$get()
    
    # Process inverse of a matirx inverse value, Cache inverse of a matirx value & Return inverse of a matirx value
    matrixVal <- solve(data, ...)
    x$setinverseval(matrixVal)
    matrixVal
}