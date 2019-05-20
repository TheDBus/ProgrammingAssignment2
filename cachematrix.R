## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix creates a matrix that can have it's inverse cached so to save computational time

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse <- function(solve) i<<-solve
  getinverse <- function() i
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}



## The cachesolve function works with the cacheMatrix function to return a cached inverse matrix or calculated matrix if the 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached value")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
