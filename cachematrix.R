## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 

##    This function set the value of the matrix, get the value of the matrix, set the value of the matrix inversion and get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The next function calculates the matrix inverse created with the above function

cacheSolve <- function(x, ...) {
inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(inv)
  inv
}
