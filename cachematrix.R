## This file Programming assignment 2 required functions
### makeCacheMatrix and cacheSolve functions
## The technique for caching data and accessing the cache are coppied from 
### the example in the assignment

## makeCacheMatrix makes a 'matrix' capable of storing its value and inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ### makeCacheMatrix$set(y) sets a new matrix and clears the inverse
    set <- function(y) {
        x<<-y
        inv<<-NULL
    }
    
    ### makeCacheMatrix$get() returns the matrix
    get <- function() x
    
    ### makeCacheMatrix$setinv(i) sets the inverse matrix
    setinv <- function(i) inv<<-i
    
    ### makeCacheMatrix$getinv() returns the inverse matrix
    getinv <- function() inv
    
    ### return line
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve checks for a cached inverse
## if a cached inverse doesn't exist the inverse is calculated and cached
cacheSolve <- function(x, ...) {
    ### get cached inverse
    inv <- x$getinv()
    ### if inv exists then return
    if(!is.null(inv)) {
        message("retrieving cached data")
        return(inv)
    }
    
    ### calculate the inverse and cache the value
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
    ### return calculated value
    inv
}
