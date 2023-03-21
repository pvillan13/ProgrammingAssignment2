## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatric is a function that is intended to make a special "matrix" object that will have
## the ability to cache the inverse of an input

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y){
    x<<-y
    inv <<-NULL
  }
  get <- function()x      #function to get matrix x
  setinverse <- function(inverse) inv <<-inverse
  getinverse <- function()inv
      
      list(set=set, get=get, 
      setinverse=setinverse, 
      getinverse=getinverse)
  )
}


## Write a short comment describing this function
## The cacheSolve function computes the inverse of the special "matrix" as returned by 
## the function makeCacheMatrix. cacheSolve aims to retrieve the inverse of the cache once 
## the inverse is already calculated, assuming it has stayed the same

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse() 
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
  }

