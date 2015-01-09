##
## Matrix object with cacheable solve
## By odeleongt, based on the makeVector and cachemean examples provided in
## the instructions of the Programming Assignment 2, rprog-007 class in Coursera
##
## The functions can create a cacheable matrix object which can "remember" if it
## already knows its inverse and avoid calculating it again. The object can only
## be used (directly) with the function cacheMatrix.


## The function makeCacheMatrix creates a special object which stores a matrix
## and provides methods to get or set the matrix value, and to set or get the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # Function to set the value of the object (e.g. cached$set(matrix()))
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Function to get the value of the object (e.g. cached$get())
  get <- function() x
  
  # Function to set the inverse of the object (e.g. cached$setsolve(solve(matrix())))
  setsolve <- function(solve) m <<- solve
  
  # Function to get the inverse of the object (e.g. cached$getsolve())
  getsolve <- function() m
  
  # Return the special object, which consists of a list of the methods
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function calculates or gets the inverse of a matrix created with
## makeCacheMatrix. If the object already knows its inverse, the function gets
## it and avoids performing the calculation. Otherwise, it uses solve to get the
## inverse. For this function to work the matrix needs to be square 
## (e.g. `solve(1:6, ncol=2)` does not work) and not exactly singular 
## (e.g. `solve(matrix(1:16, ncol=4))` does not work). See the included
## reproducible examples below.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If there was no cached results, calculate them, using the objects methods
  # to get the data and set the inverse.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}



## Include some minimum reproducible examples, you can use these examples (after
## runing the code which defines the makeCacheMatrix and cacheSolve functions)
## to thest that the functions work (and that there is indeed a speedup when
## using the cached inverse).


# Test matrix
m <- 1024
sm <- matrix(sample(1:(m*m)), ncol=m)
cached <- makeCacheMatrix(sm)

# Compare times
system.time(solve(sm))
system.time(cacheSolve(cached))
system.time(cacheSolve(cached))


# Modify the matrix
cached$set(solve(sm))

# Solve
system.time(cacheSolve(cached))
system.time(cacheSolve(cached))



# End of script