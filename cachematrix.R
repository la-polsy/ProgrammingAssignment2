## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function creates a special "matrix" that can cache its inverse:
# the inverse is set null as default or when a new matrix is set
# when setInv is called, the result of the solve function is cached in the variable inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
# this function computes the inverse of the "matrix", but it first checks if there's an already cached value:
# if the getInv function returns something, then the cached inverse is still valid, so it's returned
# otherwise, the matrix is retrieved through the get function and the solve fuction on the retrieved matrix
# is passed to the setInv function to be cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
