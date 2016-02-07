## The function makeCacheMatrix creates a matrix object.    The object can cache the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y)
     { 
       x <<- y
       m <<- NULL
     }

  get <- function() x 
  setInverse <- function(solve)   m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    }



## The function cacheSolve returns the inverse of a matrix.    If the inverse of the matrix has already
## been found then the function will return the cached value. 
cacheSolve <- function(x, ...) {
  
      m <- x$getInverse()
  
          if (!is.null(m))
           {
            
            message("getting cached data")
            return(m)
          }
      
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
      
}
