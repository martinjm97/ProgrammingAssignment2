
# makeCacheMatrix creates a list that caches the inverse of a matrix.

makeCacheMatrix <- function (x = matrix()) {
  inv<-NULL            # Set initial value
  setInit <- function(y) { # Global sets
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse  
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The cacheSolve function returns the inverse of the matrix, 
# but checks first if the calue is cached

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {  # check if you have already calculated the inverse
    message("You have already calculated the inverse!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data) # Calculate the inverse
  x$setinverse(inv)  # Store the value for the future.
  return inv
}


