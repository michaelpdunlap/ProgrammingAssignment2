## Put comments here that give an overall description of what your
## functions do

## this function creates an environment 
## that will allow for the storage of a matrix
## and a list of get/set based functions to 
## set or extract values from it
## created 1/11/2020
## last updated 1/11/2020

makeCacheMatrix <- function(x = matrix()) {
     inv <-NULL
     set <-function (y) {
          x <<-y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(mean) inv <<- mean
     getinverse <- function() inv
     list (set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## Compute the inverse of the matrix
## defined within an environment of type makeCacheMatrix
## created 1/11/2020
## last updated 1/11/2020

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return (inv)
     }
     data <- x$get()
     inv <- solve (data, ...)
     x$setinverse(inv)
     inv
}
