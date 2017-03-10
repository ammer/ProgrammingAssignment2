## Programming Assignment 2: Lexical Scoping
## Refer to: https://github.com/rdpeng/ProgrammingAssignment2
## example:
##    a <- makeCacheMatrix(matrix(rnorm(100000000), 10000, 10000))
##    cacheSolve(a)

## Creates a special "matrix" object that can cache its inverse
## Notice: x must be invertible
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInversion <- function(i) im <<- i
  getInversion <- function() im
    
  list(set = set, get = get, setInversion = setInversion, getInversion = getInversion)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  im <- x$getInversion()
  if(!is.null(im)) {
    message("Got cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data)
  x$setInversion(im)
  im
}
