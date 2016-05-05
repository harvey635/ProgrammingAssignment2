## This file contains functions written to demonstrate 
## caching the inverse of a matrix

## written to satisfy the first requirement 
## of the second programming assignment for the 
## Coursera course "R Programming" taught by 
## R Peng of Johns Hopkins University

## 1. This function creates a special "matrix" object
##    that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

     m <- NULL 
     ## function to set the value of the matrix and store in cache
     set <- function(y) { 
          x <<- y 
          m <<- NULL 
     } 
     ## function to get the value of the matrix
     get <- function() x 
     ## function to invert the value of the matrix and store in cache
     setinverse <- function(inv) m <<- inv 
     ## function to get the inverted value
     getinverse <- function() m 
     
     list(set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse) 
     
}

## 2. This function computes the inverse of the special
##    "matrix" returned by `makeCacheMatrix` above. If the inverse has
##    already been calculated (and the matrix has not changed), then
##    `cacheSolve` should retrieve the inverse from the cache.
## NOTE: no requirement to provide messaging as in example 'cachemean'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     ## attempt to retreive the inverse of x from cache
     inverse <- x$getinverse() 
     ## if not in cache
     if(is.null(inverse)) { 
          ## calculate using the solve() function
          inverse <- solve(x$get())
          ## store it in cache 
          x$setinverse(inverse) 
     } 
     ## return the value
     inverse
}
