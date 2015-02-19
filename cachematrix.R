##   The function pair in this script is responsible for calculating the inverse of an 
## invertible matrix and caching the result. In caching the result, the inverse of the
## same matrix can simply be called from the cache instead of calling the inverse method
## (resource intensive operation) again. 
##   If a new matrix is introduced or the inverse result
## is not available, the script will calculate and cache the new inverse



## The makeCacheMatrix function creates a special "matrix" and caches the matrix and the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     setmat <- function(y) {  # function responsible for caching the matrix
          x <<- y
          inv <<- NULL
     }
     getmat <- function() x  # function responsible for retrieving the matrix
     setinv <- function(solve) inv <<- solve  # function responsible for caching the matrix inverse
     getinv <- function() inv  # function responsible for retrieving the matrix inverse
     list(setmat = setmat, getmat = getmat,
          setinv = setinv,
          getinv = getinv)
}


## The cacheSolve function computes the matrix inverse returned by the makeCacheMatrix function.
## Three conditions exists: (1) If the cached inverse of an existing cached matrix is not null,
## the inverse is simple called from the cache; (2) if an inverse result is available in the cache
## but a new matrix is introduced, the inverse of the new matrix is computed and cached; (3) if
## no inverse is available in the cache, the inverse is calculated and cached.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     mat <- x$getmat()
     if(!is.null(inv) && (solve(mat) == inv)) {
          message("Inverse exists and matrix not yet updated:") 
          return(inv)  # Simply return the cached inverse of the existing matrix
     }
     if (!is.null(inv) && solve(mat) != inv) {
          message("New matrix introduced:")
          data <- x$getmat()
          inv <- solve(data, ...)
          x$setinv(inv)  # Returns the inverse of the new matrix
          inv
     } else {
          message("New inverse calculated:")
          data <- x$getmat()
          inv <- solve(data, ...)
          x$setinv(inv)
          inv  # Returns and caches the inverse of the new matrix
     }             
}
