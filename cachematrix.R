## this functions implement a special type of matrix that 
## caches the result of the inverse of the matrix



# This function is the 'special matrix' with methods
# to get and set the matrix as well as the inverse result

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




## This function implements the caching mechanism
## (i.e. checks if the inverse was already calculated. If true, 
## returns this cached result. If false, calculate the inverse and cache the result)

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


