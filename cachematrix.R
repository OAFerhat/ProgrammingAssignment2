
# The function makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function calculates the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result from the cache and skips the
# computation. Otherwise, it computes the inverse, sets the value in the cache via the
# setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Test with empty and filled cache
## > x=cbind(c(1,2,3),c(4,1,5),c(6,7,1))  
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2] [,3]
## [1,]    1    4    6
## [2,]    2    1    7
## [3,]    3    5    1


## No cache yet
## > cacheSolve(m)
## [,1]        [,2]        [,3]
## [1,] -0.40476190  0.30952381  0.26190476
## [2,]  0.22619048 -0.20238095  0.05952381
## [3,]  0.08333333  0.08333333 -0.08333333

## Then retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
## [,1]        [,2]        [,3]
## [1,] -0.40476190  0.30952381  0.26190476
## [2,]  0.22619048 -0.20238095  0.05952381
## [3,]  0.08333333  0.08333333 -0.08333333



