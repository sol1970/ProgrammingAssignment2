## This program calculates the inverse of a function and cache the result.
## This program is a modified version of the one that calculates the mean
## This Program calculates the inverse of an n X n Matrix

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix  <-  function(x = matrix()) {
  inv <- NULL  # set inv value to null
  set <- function(y) {
    x <<- y
    
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ## stores the above four functions
  list(set = set, get = get,setinverse = setinverse
       getinverse = getinverse)
}

## This function calculates inverse if it it han't been calculated
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {  # checks if the inverse is calculated or not
    message("getting cached data") 
    
    return(inv)  # if inv is available, it retrieves from the cache
  }
  # calculate inverse of a matrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv  # return new value of inverse
}
