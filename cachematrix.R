## Functions makeCacheMatrix and cacheSolve are used 
## to get cached inverse of a matrix if available.
## makeCacheMatrix creates the cached matrix and cacheSolve
## returns the cached inverse of the matrix if available 
## otherwise calculates and caches and returns the inverse.


## makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to :
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  setMatrix <- function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  
  getMatrix <- function() x
  
  setInverse <- function(inverse){
    inverseMatrix <<- inverse
  }
  
  getInverse <- function() inverseMatrix
  
  list(setMatrix=setMatrix, getMatrix=getMatrix, 
       setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve returns the cached inverse of the matrix if
## available otherwise calculates the inverse of the supplied
## matrix, caches the result so that it can be used later and
## returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  
  if(!is.null(inverseMatrix)){
    message("getting cached inverse")
    return(inverseMatrix)
  }
  
  matrix <- x$getMatrix()
  inverseMatrix <- solve(matrix, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
