## This function , makeCacheMatrix creates a special matrix,
## which is a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## The function creates a structure which holds the matrix along with its inverse


makeCacheMatrix <- function(x = matrix()) {
  ## variable to store the inverse of the input matrix
  vInverse <- NULL
  
  ## function to store the input matrix
  set <- function(y) {
    x <<- y
    vInverse <<- NULL
  }
  
  ## function to obtain the input matrix
  get <- function() {
    x
  }
  
  ## function to store the calculated inverse matrix
  setInverse <- function(inverse) {
    vInverse <<- inverse
  }
  
  ## function to obtain  the calculated inverse  matrix
  getInverse <- function() {
    vInverse
  }
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
  
}


## The following function calculates the inverse of the special
## "matrix" created with the makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  ## check if the inverse is already calculated and available in the
  ## cache
  if (!is.null(inverse)) {
    message('Getting inverse from cache')
    return(inverse)
  }
  
  ## The inverse is not obtained from the cache .
  ## So doing a computation of the inverse
  matrixToInvert <- x$get()
  inverse <- solve(matrixToInvert, ...)
  
  ## Storing the calculated inverse in the cache
  x$setInverse(inverse)
  inverse
}
