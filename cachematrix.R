## Creates pseudo-matrix with cache properties
## and defines the funtions to handle its values & cache inverse

## Creates pseudo-matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  m <- x
  
  set <- function(x = matrix()) {  
    m <<- x                                 # sets the values of mtrx in m
    inv_m <<- NULL                          # and resets cache of inverse
  }
  
  get <- function() m                       # returns value of matrix
  
  set_inv <- function(inv) inv_m <<- inv    # sets the cache value of inverse
  
  get_inv <- function() inv_m               # gets the cache value of inverse
  
  
  return(list(set = set, get = get, set_inv = set_inv, get_inv = get_inv))

}

## Calculates/retrieve inverse of pseudo-matrix

cacheSolve <- function(x, ...) {
  inv_m <- x$get_inv()                      # gets the inv value from cache
  if (!is.null(inv_m)) {                    # if previusly calculated, only prints msg
    message("Getting inverse from cache!")  
  }
  else {                                    # if not yet stored, calculates & sets
    m <- x$get()
    inv_m <- solve(m)
    x$set_inv(inv_m)
  }
  return(inv_m)
}
