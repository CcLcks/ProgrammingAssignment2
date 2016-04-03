# Description: In this assignment, I will create 2 functions which can store a matrix and caches its inverse. 

# The first function, makeCacheMatrix, can create a special matrix. It contains a function to: 1) set the value of the matrix; 2) get the value of the matrix; 3) set the value of the inverse; 4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

# The second function calculated the inverse of the special matrix created with the above function. It can first check to see if the inverse has beeen calculated. If so, it gets the inverse from the cache and skips the calculation. Otherwise, it calculates the inverse and sets the value of the inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
