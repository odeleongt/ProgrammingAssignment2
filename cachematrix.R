## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
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


## Write a short comment describing this function

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



## Include some minimum reproducible examples


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