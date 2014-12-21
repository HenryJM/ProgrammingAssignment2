## The first function creates a special object that can be used to store a matrix and cache its inverse.
## The second function returns the cached inverse if it is available, else evaluates it.
## They are made to resemble the example functions as closely as possible, except they work on
## matrices and the inverse, rather than vectors and the mean.

## This function creates a special "matrix" which is a list of functions to get/set the matrix/its inverse.
## Like the example, it assumes that setInv() will not be misused to illegally change the value of inv (i.e. 
## setInv() will only be used in cacheSolve).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function takes a "matrix" object like the one created by makeCacheMatrix and gives the inverse. It will
## take it from the cache if it is avaiable. Otherwise, it will evaluate the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
