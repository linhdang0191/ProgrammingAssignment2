
## Those pair of functions caches the inverse of a matrix to improve time
## efficiency in time-consuming computations. Instead of repeateadly calculating
## the result, we cache the past compuation in a special object, and look it up
## in the cache if the input does not change. 

## makeCacheMatrix creates a special object - a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse 
## 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv 
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve first check whether the inverse of the input matrix has been
## computed. If it has been computed, it returns the inverse.

## Otherwise, it computes the inverse using solve(), and set the value of
## the inverse in cache using setinverse. 

## We assume that x is always an invertible, square matrix (n by n)

cacheSolve <- function(x, ...) {
  
   inv <- x$getinverse()
   
   if (!(is.null(inv))) {
     message ("Getting cached data...")
     return(inv)
   }
   
   data <- x$get()
   inv <- solve(data,...)
   
   x$setinverse(inv)
   inv 

}
