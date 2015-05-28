# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(myMatrix = matrix()) {
     myInvers <- NULL
     set <- function(x) {
          myMatrix <<- x
          myInvers <<- NULL
     }
     get <- function() myMatrix
     setinverse <- function(inverse) myInvers <<- inverse
     getinverse <- function() myInvers
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
     intrnInverse <- x$getinverse()
     if(!is.null(intrnInverse)) {
          message("getting cached data.")
          return(intrnInverse)
     }
     myMatrix <- x$get()
     intrnInverse <- solve(myMatrix)
     x$setinverse(intrnInverse)
     intrnInverse
}


