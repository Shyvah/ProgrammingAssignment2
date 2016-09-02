## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix builds a set of functions - set(), get(), setsolve(), geetsolve() - and two data objects, x and m.
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set() assigns input argument to x and  NULL to m in parent environment. 
  ## Therefore, if there is already a valid matrix cached in m, whenever x is reset, 
  ## the value of m cached in the memory of the object is cleared, 
  ## forcing subsequent calls to cachesolve() to recalculate the inverse matrix 
  ## rather than retrieving the wrong value from cache.
 
   set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: This function  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
