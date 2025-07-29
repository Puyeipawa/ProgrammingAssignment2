## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                 # Cached inverse. Will be empty at fist.
  
  set <- function(y) {
    # Assigns a new matrix and clears any cached inverse
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() {
    inv
  }
  
  # Expose the four functions as a list
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes or retrieves the cached inverse of the special matrix object above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("pls wait, getting cached data")
    return(inv)
  }
  m <- x$get()
  
  # Defensive checks
  if (!is.matrix(m)) stop("Your input is not a matrix")
  if (nrow(m) != ncol(m)) stop("The matrix must be square to invert")
  
  # Compute, cache, and return the inverse
  inv <- solve(m, ...)
  x$setinverse(inv)
  inv
  
}
