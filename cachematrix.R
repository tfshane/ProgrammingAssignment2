## Taking the inverse of a matrix can be time consumming
## To save time it is beneficial to cache the inverse matrix 
## in order to not calculate it every time it is needed.

## makeCacheMatrix() 
## creates functions for setting a matrix and it's inverse to the global environment
## as well as the ability to get a matrix and it's inverse from the global environment
## INPUT: nothing
## OUTPUT: 4 functions:: set(), get(), setSolve() and getSolve()
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the "solve" variable to NULL
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  
  ## return a special "matrix" of four functions
  list(set = set, get = get,
       setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve() 
## First time it is called should calculate the inverse matrix and cache it for future use 
## Successive calls pulls the inverse matrix from the cache to save time
## INPUT: the matrix and the functions from makeCacheMatrix()
## OUTPUT: inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  ##return matrix inverse
  s
}
