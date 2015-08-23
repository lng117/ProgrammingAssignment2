## Caching the Inverse of a Matrix
## with 2 functions:
## makeCacheMatrix() 
## cacheSolve()
##
## How to run @R:
## > a <- matrix(1:4, nrow=2, ncol=2)   ## good case
## > b <- matrix(1:6, nrow=3, ncol=2)   ## bad case
##
## > tmp <- makeCacheMatrix(a)
## > cacheSolve(tmp)


## This function creates a special matrix, which is a list containig a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse value of the matrix
## 4. get the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y)  {
    x <<- y     ## assign value to an object in an env
    m <<- NULL  ## that is diff from the current env
    
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse 
  getInverse <- function() m
  list(set = set, get= get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"
## returned by the function: makeCacheMatrix:
## 1. check the inverse has already been calculated, return the inverse from the cache
## 2. otherwise, vaidate the matrix to be a SQUARE matrix for inversion
## 3. If it is not a square matrix, show error message with original matrix given
## 4. If it is a square matrix, calculates, caches and returns the inverse
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  
  data <- x$get()
  ## to validate x = a square matrix
  if (nrow(data) != ncol(data)) {
    message("A square Matrix is mandatory for Matrix Inversion")
    data
      } 
  else {
    m <- solve(data, ...)
    x$setInverse(m)
    m
  }
}

