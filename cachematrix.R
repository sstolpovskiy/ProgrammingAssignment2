## Put comments here that give an overall description of what your
## functions do

## Function wrap passed matrix with complex object contains 4 new methods:
## 1. set - set the value of the matrix
## 2. get - get the value of the metrix
## 3. setInversedValue - set the value of the solve function
## 4. getInversedValue - get the value of the solve function 
makeCacheMatrix <- function(x = matrix()) {
  cachedInversed <- NULL
  set <- function(y) {
    x <<- y
    cachedInversed <<- NULL
  }
  get <- function() x
  setInversedValue <- function(invertedValue) cachedInversed <<- invertedValue
  getInversedValue <- function() cachedInversed
  list(set = set, get = get,
       setInversedValue = setInversedValue,
       getInversedValue = getInversedValue)
}



## Function returns cached inverse value from passed matrix
## If matrix don't have cached value it compute it, 
## save computed value into matrix object and return computed value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversedValue <- x$getInversedValue()
        if(!is.null(inversedValue)) {
          message("getting cached data")
          return(inversedValue)
        }
        data <- x$get()
        inversedValue <- solve(data, ...)
        x$setInversedValue(inversedValue)
        inversedValue
}
