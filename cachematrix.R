## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
  
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

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
