## Below are two functions that are used to create a special object that stores 
## a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
## It contains additional boolean variable which indicates if the inverse 
## matrix value should be recomputed.
## The function returns a list of functions which:
##    1.  set the value of the matrix and set the value of the boolean variable 
##        comp to TRUE
##    2.  get the value of the matrix
##    3.  set the value of the inverse matrix
##    4.  get the value of the inverse matrix
##    5.  get the value of the comp variable

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      comp <- TRUE
      set <- function(y) {
            x <<- y
            i <<- NULL
            comp <<- TRUE
      }
      get <- function() x
      setinverse <- function(inverse) {
            i <<- inverse
            comp <<- FALSE
      }
      getinverse <- function() i
      getcomp <- function() comp
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse,
           getcomp = getcomp)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated and if the matrix has not changed. 
## If so, it `get`s the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse and the comp variable (to mark that there is no need to compute
## the inverse next time) in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i) && !x$getcomp()) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
