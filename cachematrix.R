## These are functions that cache the inverse of a matrix

## This function creates a special matrix used to cache
## the inverse of a matrix "x"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes for the inverse of the special matrix stored by makeCacheMatrix 
## function above. If the inverse has already been calculated, then this function
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

## Testing the functions by getting the inverse of the matrix below
eh <- makeCacheMatrix(matrix(c(4,6,2,8),2,2))
eh$getinv()
cacheSolve(eh)


## Making sure that the inverse is correct, by getting the inverse 
## of the inverse which is the original matrix input
oh <- makeCacheMatrix(cacheSolve(eh))
oh$getinv()
cacheSolve(oh)
