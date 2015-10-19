## Function creating a special object that stores a matrix and it's inverse matrix cached

## Creating the object for a given matrix

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returning the inverse matrix if it was cached 
##or calculating it, if it was not. Assuming that the matrix is always invertible

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) { 
      message("getting cached matrix")
      return(m)
  }
  data <-x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
