## Below are two functions that are used to create a special object 
## that stores a matrix and its inverse

## The makeCacheMatrix function returns a list containing 
## four functions for getting and setting the cached matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function returns a matrix that is the inverse of 'x'.
## The argument 'x' should be the kind of object returned by makeCacheMatrix.
## The first time cacheSolve is called on any given such object, 
## the inverse of the object matrix is computed, cached and returned.
## If cacheSolve has already been called on the (unmodified) object before,
## then the cached inverse will be returned and a message will be displayed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mtrx <- x$get()
  inv <- solve(mtrx, ...)
  x$setinv(inv)
  inv   
}

