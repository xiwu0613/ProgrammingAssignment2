##There are two functions described in this file. The goal is to return the inverse
## of a matrix.



##1. The function makeCacheMatrix is to provide a list of methods to change status
##of some variables. These methods are mainly called in the other function cacheSolve.
## The list includes:1)set the value of the matrix 2)get the value of the matrix
## 3)set the inverese of the matrix 4)get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## The function cacheSolve calculates the inverse of input matrix. 
## To save time, the function will firstly check if the result has been cached.
## If the result has been cached, function cacheSolve will return the result which
## has been cached. Otherwise, it will calculate the inverse of input matrix and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
