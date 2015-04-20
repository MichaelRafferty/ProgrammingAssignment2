## This file contains four functions
## 1 & 2) makeCacheMatrix and cacheSolve functions required by the assignment
## 3 & 4) example functions from the assignment

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

## cacheSolve checks for a cache'd inverse
## if a cached inverse doesn't exist the inverse is calculated and cached
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    ### if inv exists then return
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ### calculate the inverse and cache the value
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
    ### return calculated value
    inv
}


## example functions from the asssignment
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) { 
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setmean <- function(mean) m <<-mean
    getmean <- function() m
    list(set =set, get=get, setmean=setmean, getmean=getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
