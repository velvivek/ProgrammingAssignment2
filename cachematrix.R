## Put comments here that give an overall description of what your
## functions do

## This function creates list for getting the values for inversion of matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inversion <- NULL
  set <- function(y) {
    x <<- y
    inversion <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversion <<- inverse
  getinverse <- function() inversion
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## This function generates the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  inversion <- x$getinverse()
  if(!is.null(inversion)) {
    return(inversion)
  }
  data <- x$get()
  inversion <- solve(data)
  x$setinverse(inversion)
  inversion
}

