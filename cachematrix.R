## These functions allow to cache an inverse of a matrix if it hass already been calculated
## and then retrieve it from cache. This should give a computational benefit

## This function creates a special "matrix" object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      ## If the matrix is changed - clear cache
      inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    
    ## Creating a special "matrix" object
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()
  if(!is.null(i)) {
    ## Return inverse of 'x' from cache
    message("getting cached data")
    return(i)
  }
  
  ## Calculate inverse of 'x', save it to object, return it
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
