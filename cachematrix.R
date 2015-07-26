## This is a pair of functions intended to create a matrix object
## and calculate its inverse

## This function creates a list that will set a matrix's value, get the value
# sets the inverse, and gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the matrix generated
## by makeCacheMatrix. If it has been calculated it will not
## recalculate it. It will instead just pull the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) { #if m is not null then it will return its value
    message("getting cached data")
    return(m)
  }  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
