## This program demonstrates a way to cache a potentially
## time consuming computation, namely, the calculation of 
## the inverse of a matrix. The inverse is only calculated 
## the first time and when the matrix changes, otherwise 
## a cached value is returned. 

## The makeCacheMatrix creates a "cacheable" matrix
## that is matrix associated with a list of functions 
## which perform the following action:
## 1. sets the matrix  2. gets the matrix
## 3. sets the inverse 4. gets the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    } 
    
    getMatrix <- function() x
    
    setInverse <- function (inverse) inv <<- inverse
    
    getInverse <- function () inv
    
    list (setMatrix = setMatrix, getMatrix = getMatrix,
          setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function returns the cached value 
## of the inverse if available, 
## otherwise if calculates the inverse, caches this 
## newly computed inverse value and returns this 
## inverse value to the caller 

cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
        message ("returning cached inverse")    
        return (inv)
    }
    
    mat <- x$getMatrix()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
}
