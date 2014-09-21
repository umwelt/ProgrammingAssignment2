## Put comments here that give an overall description of what your
## functions do

##As Matrix inversion is an operation that potentially has an elevated 
##computational cost, there may be some benefit on caching the Inverse of a Matrix
##rather than compute it every time.

## Write a short comment describing this function

##makeCacheMatrix creates a list that contais a function for
## 1- set the value of the Matrix
## 2- get the value of the Matrix
## 3- set the value of inverse of the Matrix
## 4- get the value of inverse of the Matrix


makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##This following function asumes that the Matrix is always invertible
##Returns the inverse of a Matrix setting the valus in the cache via setinverse function
##if the Matrix was already computed, gets the result
##and skips the computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
