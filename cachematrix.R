## makeCacheMatrix creates a special matrix that
## also stores its own inverse (once computed)
##
## cacheSolve takes one of these special matrices as
## input and returns its inverse, either from the
## cached inverse or by computing it


## makeCacheMatrix
## Makes a special type of matrix that caches its own
## inverse (when computed via cacheSolve)
## The return type is a list of functions for accessing
## and modifying the matriz

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  
  ## Setting function
  set <- function(y){
    x <<- y
    xInverse <<- NULL
  }
  
  ## Accessor function
  get <- function() {x}
  
  ## Setting function for inverse matrix
  setInverse <- function(inv) {xInverse <<- inv}
  
  ## Accessor function for inverse matrix
  getInverse <- function() xInverse
  
  ## Return a list containing the four functions defined above
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve
## When called on a matrix created using the makeCacheMatrix
## function, returns the inverse matrix.
## This is done from the cache if the inverse has been computed
## previously. If not, the inverse in computed and stored.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  ## Do we have a cached inverse?
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  ## If we don't have a cached inverse, compute it...
  inv <- x$get()
  inv <- solve(inv)
  ## ...and cache it
  x$setInverse(inv)
  
  ## Return the inverse we have just computed
  return(inv)
}
