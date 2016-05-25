## Function makeCacheMatrix creates a list of functions to set a matrix, get it, 
##set inverse and get inverse. 
## When result is assigned to a variable and that variable
## is used as argument in cacheSolve (or directly nesting to functions in same command),
## this last function first attempts to retrieve
## result from cache. If result has not been calculated, it calculates. If it is
## found in cache, then value is retrieved and you save calculating steps

## MakeCacheMatrix returns a list of functions 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y ##this establishes 
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) inv <<- matinv
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##This function first attempts to get inverse of function from caché 
##If it gets it, then, avoids calculating by returning vale
##if does not get it. Then calculates

cacheSolve <- function(x, ...) {
inv <- x$getinv() ##if setinv was previously ran with variable inv(ie calculated),
                  ##inv would not be null
  if (!is.null(inv)){
  message("getting cached data")
  return(inv)
}
matnew <- x$get() ##this step is just to have a more clear code
inv <- solve(matnew) ##this could be resumen with previous line (x$get as solve arg)
x$setinv(inv) ## setinv defines inv in other environment
return(inv)
}
