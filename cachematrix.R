## makeCacheMatrix is a function creating an environment
## and a list of functions for a special type of matrix
## "CacheMatrix"
## This function can be used, for example, in the following way :
## A <- makeCacheMatrix( matrix(c(2,3,1,7), nrow = 2, ncol = 2))

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) inverse <<- inv
      getinverse <- function() inverse
      list(set = set, get = get, 
           setinverse = setinverse, 
           getinverse = getinverse)
}


## cacheSolve updates the CacheMatrix x by computing
## the inverse of x and storing it in the internal variable inverse
## In the end, cacheSolve returns the inverse
## this function can be called this way : cacheSolve(A) with A 
## constructed with the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}

A <- makeCacheMatrix( matrix(c(2,3,1,7), nrow = 2, ncol = 2))
A$getinverse()
cacheSolve(A)
A$getinverse()