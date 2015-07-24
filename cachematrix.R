## makeCacheMatrix is a function which can build a matrix and which can cache its inverse




makeCacheMatrix <- function(x = matrix()) {

  m <- NULL 
  set <- function(y){
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is a Matrix which can either return the cached inverse matrix or compute the inverse of the Matrix and return this inverse



cacheSolve <- function(x, ...) {
  m <- x$getinverse() # function is trying to get the cached inverse of the matrix from the makeCacheMatrix
  if(!is.null(m)){
    message("getting inverse data")
    return(m) # if inverse cached matrix could be obtained from makeCacheMatrix, return the message "getting inverse data" and return the cached matrix
  }
  else {
  data <- x$get() # get matrix from makeCacheMatrix function
  m <- solve(data,...)# compute the inverse of the matrix
  x$setinverse(m)# write the inversed matrix
  m # return the matrix
  }   
}
