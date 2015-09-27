
## makeCacheMatrix will create a 'matrix, which is able to cache its value
## This implementation will use solve() function for matrix inversion
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)  
  
 }




##cacheSolve function will do the actual inversion for the cached matrix which
##is created first by calling makeCacheMatrix() function


cacheSolve <- function(x, ...) { 
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
  
  ## Return a matrix that is the inverse of 'x' 
  } 
