## This function will create container object that is used to store a matrix
## and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(value) inv <<- value
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function will determine whether the inverse of square matrix x has been
## sloved already. If that's the case, will return the inverse of x. Otherwise,
## this function will compute the inverse of x and store in the special container
## to return it later
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return (inv)
    }
    data <- x$get()
    inv <- solve(data, diag(dim(data)[1]), ...)
    x$setinverse(inv)
    inv
}

# quick unit test
cacheSolveTest <- function(...) {
  m <- matrix(c(9, 8, 7, 6, 5, 4, 3, 2, 0),  nrow=3,  ncol=3)
  print(m)
  cm <- makeCacheMatrix(m)
  invTest1 <- cacheSolve(cm, ...)
  invTest2 <- cacheSolve(cm, ...)
  identical(invTest1, invTest2)
}
