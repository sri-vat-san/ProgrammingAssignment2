## Programming Assignment 2: caching the inverse of a matrix
## Below are a pair of functions that are used to create a special matrix object 
## that stores a matrix and caches its inverse.

## Results from a sample test for these functions
## > source('cachematrix.R')
## > my_matrix$set(matrix(c(2, 2, 3, 7), 2, 2))
## > my_matrix$getinverse()
## NULL
## > cacheSolve(my_matrix)
## [,1]   [,2]
## [1,]  0.875 -0.375
## [2,] -0.250  0.250
## > cacheSolve(my_matrix)
## getting cached data
## [,1]   [,2]
## [1,]  0.875 -0.375
## [2,] -0.250  0.250
## > my_matrix$getinverse()
## [,1]   [,2]
## [1,]  0.875 -0.375
## [2,] -0.250  0.250

## This function creates a special matrix object that can cache its inverse.
## Essentially, we do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of its inverse (assuming it is invertible)
## 4. get the value of its inverse (assuming it is invertible)

makeCacheMatrix <- function(x = matrix()) {
    # 'inv' stores the cached value, we initialize it to NULL
    inv <- NULL
    
    # creating matrix in the work environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # fetch the value of matrix
    get <- function() x
    
    # invert the matrix and store it in 'inv' (i.e. cache)
    setinverse <- function(inverse) inv <<- inverse
    
    # get the inverse of the matrix from 'inv' (i.e. cache)
    getinverse <- function() inv
    
    # list of created functions in the working environment
    list(set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special matrix created with the 'makeCacheMatrix' function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    ## (if it exists) return inverted matrix from cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## fetch the matrix
    data <- x$get()
    
    ## if X is a square invertible matrix, then solve(X) returns its inverse
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
