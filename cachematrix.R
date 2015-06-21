## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache it's inverse.

makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mat <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mat);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special "matrix" returned 
## by 'makeCacheMatrix' above. If the inverse has already been
## calculated (and the matrix has not changed), then
## 'cacheSolve' should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
  inverse <- mat$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mat$get()
  inverse <- solve(data, ...)
  mat$setinv(inverse)
  return(inverse)
}

################
## Sample run ##
################

# > x <- matrix(rnorm(9), nrow = 3)           // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, so return
#                                             // the cached inverse