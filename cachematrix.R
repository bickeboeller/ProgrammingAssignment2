## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## `makeCacheMatrix` creates a special "vector", which is
        ##  really a list containing a function to
        ##  
        ##  1.  set the value of the vector
        ##  2.  get the value of the vector
        ##  3.  set the value of the inverse matrix
        ##  4.  get the value of the inverse matrix
        m <- NULL
        set <- function(y) {
              x <<- y
              m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## is the cached value was not found, it recalculates the matrix
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
