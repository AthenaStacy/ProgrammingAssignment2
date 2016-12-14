## As prompted in the homeworkd, makeCacheMatrix creates a special 
## matrix object that can cache its inverse

## Below is an example of how to define a matrix and use these 
## functions to get the matrix inverse:
## > source('cachematrix.R')
## > z <- matrix( c(1, 2, 3, 4), nrow=2, ncol=2)
## > y <- makeCacheMatrix(z)
## > a <- cacheSolve(y)


makeCacheMatrix <- function(x = matrix()) {

     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     set_inverse <- function(solve) m <<- solve
     get_inverse <- function() m
     list(set = set, get = get,
          set_inverse = set_inverse,
          get_inverse = get_inverse)

}


## Also as prompted in the homework, cacheSolve computed the 
## inverse of the special "matrix" returned by makeCachematrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

     m <- x$get_inverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$set_inverse(m)
     m

}
