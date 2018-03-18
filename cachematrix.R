## Globally, thess two functions inverse the matrix and cache the result.

## makeCacheMatrix function create a vector enabling set the value of matrix, get
## the value of matrix, set the inverse of matrix, get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## In this function, matrix will be inversed only when it has not been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting inverse matrix")
    return(i)
  }
  data <- x$get()
  if (det(data) ==0) {
    print ("inverse matrix does not exist!")
  }
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
