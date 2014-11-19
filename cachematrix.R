## The 2 following functions work together to cache and retrieve 
## the inverse of a matrix x

## makeCacheMatrix creates a special "matrix" which
## is really a list of functions that:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

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


## cacheSolve calculates the inverse of the special "matrix" created
## with the above function. It first checks if the inverse is not null.
## If it is not null, it returns the inverse of the cache and skips the
## calculation; if it is null, it calculates the inverse of the data 
## sets the value of the inverse in the cache via the setmean function
## and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
    ## Return the inverse previously calculated and stored in the cache
  }
  data <- x$get()
  inv <- solve(data, ...)
  ## Calculate the inverse of the matrix
  x$setsolve(inv)
  ## Sets the value of the inverse in the cache via setmean
  inv
}

