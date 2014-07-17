## This is a set of functions that generates (if needed) and caches the inverse 
## of a matrix. The first function declares an object that includes the matrix
## and its cached inverse. The second function accesses the object, and retrieves
## the cache or inverts the matrix if the cache is null

## This function creates an object of the kind makeCacheMatrix
## This object is a listing of an environment with a matrix, its inverse if
## previously calculated (else null) and the functions needed to generate  
## the matrix inverse or retrieve the cache

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx <<- inv
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the inverse of a matrix for which a cache object has been
## created using the makeCacheMatrix function. If the cache is null, the inverse 
## of the matrix is calculated, else the inverted matrix in the cache is returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
  
}
