## Assignment 2: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. 

## Function 1: makeCacheMatrix- This function creates a special
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(m) {
      x <<- m
      invm <<- NULL
    }
    get <- function() x
    setInv <- function(i) invm <<- i
    getInv <- function() invm
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Function 2 : cacheSolve -This function computes the inverse  
## of the special “matrix” returned by makeCacheMatrix above.  
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        ## Check if the inverse has been calculated
        if(!is.null(inverse)){
          message("retrieving the cached matrix")
          return(inverse)
        }
        else{
          m <-x$get()
          inverse <- solve(m, ...)
          x$setInv(inverse)
          return(inverse)
        }
}
