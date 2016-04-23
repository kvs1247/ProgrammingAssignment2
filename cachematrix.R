## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function enables the user to make the special matrix to cache the inverse. 
# It has get, set, getinv and setinv functions. 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse1) inv <<- inverse1
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}



## Write a short comment describing this function

## This function returns a matrix that is the inverse of 'x'

cachesolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}



