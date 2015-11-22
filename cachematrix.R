## Programming Assignment 2: caching the inverse of a matrix
## Below are a pair of functions that are used to create a special matrix object 
## that stores a matrix and caches its inverse.

## This function creates a special matrix object that can cache its inverse.
## Essentially, we do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of its inverse (assuming it is invertible)
## 4. get the value of its inverse (assuming it is invertible)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special matrix created with the 'makeCacheMatrix' function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
