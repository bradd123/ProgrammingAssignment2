## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix return a list in which each element is a function

makeCacheMatrix <- function(x = matrix()) {
      ## initially NULL, holds cached value
      cache <- NULL
      
      ## stores the matrix
      setMatrix <- function(y) {
            x <<- y
            
            cache <<- NULL
      }
      
      ## get stored matrix
      getMatrix <- function() {
            x
      }
      
      ## cache the argument
      setInverse <- function(s) {
            cache <<- s
      }
      
      ## get cached value
      getInverse <- function() {
            cache
      }
      
      ## return all functions
      list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## Calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get cached value
        inverse <- x$getInverse()
        # check whether cached value exists or not
        # if exists return the value
        if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
        }
        # else get matrix, calculate inverse and store cache
        data <- x$getMatrix()
        inverse <- solve(data)
        x$setInverse(inverse)
        # return the inverse value
        inverse
}
