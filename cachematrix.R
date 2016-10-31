##  Author:       Paul Loh
##  Date:         31st Oct 2016
##  Description:  Coursera Data Science Specialisation, Wk 3 Assignment 2

##  These functions used together allow the user to create a 'special' matrix via the makeCacheMatrix function
##  This 'special' matrix is contained in a list object together with properties to get\set the matrix data and a 
##  pointer to the Solve function used to compute the inverse.
##  The makeCacheMatrix function assumes the matrix passed as argument x is invertible

# Special Matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  
  # Check if the matrix passed has changed to the data cached
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setinverse(s)
  s
}
