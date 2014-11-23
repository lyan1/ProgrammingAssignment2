## These functions caches the inverse of a matrix so that it does not have to be computed
## repeatedly, which is time-consuming.

## The makeCacheMatrix function creates a special matrix object.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function calculates the inverse of the special matrix created above. 
## if it has been calculated, however, the inverse will be readed from the cache and the computation is skipeed.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
