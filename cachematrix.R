## Put comments here that give an overall description of what your
## functions do

## Functions to cache inverse of a matrix 

## Write a short comment describing this function

## creation of matrix object
Library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialization
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function(){
    inver <- ginv(x)
    inver%*%x
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function
## this is used to get the cache data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

