## Put comments here that give an overall description of what your
## functions do

## Create matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes inverse of matrix object
cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
