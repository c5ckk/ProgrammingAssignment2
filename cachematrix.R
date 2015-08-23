##The first function, makeCacheMatrix creates a special "matrix"

  makeCacheMatrix <- function(x = matrix()) {
    Inverse <- NULL
    set <- function(y) {
      x <<- y
      Inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(matrix) Inverse <<- matrix
    getInverse <- function() Inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }



##The following function calculates the Inverse of the special "matrix" created with the above function


cacheSolve  <- function(x, ...) {
  
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  
  data <- x$get()
  I <- solve(data)
  x$setInverse(I)
  I
        ## Return a matrix that is the inverse of 'x'
}
  
