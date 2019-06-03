
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  inverse <- solve(x)
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inv(data, ...)
  x$setinv(inv)
  inv
}

## Solution
## i <-matrix(rnorm(9),3,3)
## i1 <- makeCacheMatrix(i)
## cacheSolve(i1)
## getting cached data
## [,1]      [,2]         [,3]
## [1,] 0.4227016 -1.146868 -0.009736442
## [2,] 3.5642373 -4.075050 -0.905512246
## [3,] 2.1406901 -2.973589  0.448567533