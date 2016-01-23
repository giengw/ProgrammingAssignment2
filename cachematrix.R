## Create a special matrix object that can cache its inverse

## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special matrix returned by
## makeCacheMatrix; If inverse has already been calculated
## (and matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache

## Assume that the matrix is always invertible
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
          message("getting cached data")
          return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
