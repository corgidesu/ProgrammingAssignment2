## @Author corgidesu
## Merry Xmas and Happy New Year to you and your family :D
## You can test this function by uncommenting test code below

## 
## Wrap a matrix into a cacheable matrix result object using the list construct
## This function accepts a matrix object which represents the original value for calculation
##
## It maintains an 'evaluated' object (m) in memory representing the calculated value
## If the value is changed through the 'set' method, the evaluated object (m) is cleared
## and a new value must be calculated and set
## 
## The cached object exposes the functions:
## * set : set the cached value. When setting a matrix value, the cache is invalidated
##         meaning a new cached value must be re-calculated
## * get : get the original matrix value
## * getCache: return the cached evaluated matrix value, or NULL if the original value is changed 
## * setCache: set the existing cache value
##
## Returns: a cached matrix value and its evaluated form, which becomes invalidated when
## a new matrix value is set
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    message("Clear")
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setCache <- function(inverse) {
    message("Setting cache")
    m <<- inverse
  }
  
  getCache <- function() m
  
  list(set=set, get=get, setCache=setCache, getCache=getCache)
}


##
## Solve the inverse of a cacheable matrix (x)
## This function accepts a cacheable matrix (x) and returns either a 
## cached result for the inverse of the matrix, or if the matrix has changed
## since last evaluation, it will invert the matrix, cache the new result, and 
## return it
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getCache()
  
  if (!is.null(m))
  {
    message("Returning cached")
    return(m);
  }
  data <- x$get()
  m <- solve(data)
  x$setCache(m)
  return(m)
}

##
## Testing code (uncomment for testing the cacheSolve function)
##
## x <- makeCacheMatrix(x = matrix(c(1,2,3,4), nrow=2, ncol=2))
## cacheSolve(x) # should print 'Setting cache' meaning it is evaluating the inverse
## cacheSolve(x) # should print 'Returning cached' meaning it is not evaluating and simply returning previous results
## cacheSolve(x) # should print 'Returning cached' meaning it is not evaluating and simply returning previous results
##


