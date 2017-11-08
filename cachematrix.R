## This function , makeCacheMatrix creates a special matrix, 
## which is a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## The function creates a structure which holds the matrix along with its inverse


makeCacheMatrix <- function(x = matrix()) {
  vInverse <- NULL
  set <- function(y) {
    x <<- y
    vInverse <<- NULL
  }
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    vInverse <<- inverse
  }
  
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
  if (!is.null(inverse)) {
    message('Getting inverse from cache')
    return(inverse)
  }
  matrixToInvert <- x$get()
  inverse <- solve(matrixToInvert, ...)
  x$setInverse(inverse)
  inverse
}
