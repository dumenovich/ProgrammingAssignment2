## 
# This function puts the supplied matrix into the envelope which later will be used
# to calculate the inversed matrix and save it in this envelope.
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## this function returns x which is available from the scope of the parent functions
  ## due to lexical scoping  
  get <- function() x

  ## this function is called to set the inversed matrix    
  setinv <- function(inv) m <<- inv

  ## this function is called to set the matrix    
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  ## this function return the inversed matrix or NULL if the one is not available
  getinv <- function() m
  
  ## returning the envelope as a list which contains 
  ## the functions defined above and the original matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##
#  This function calculates the inverse matrix for a given
#  source matrix and caches the result for the future use.
#
#  It accepts an envelope of the list type which contains
#  the source matrix and the utility functions which are used
#  to get and save the inversed matrix.
#
#  Before the calculation it checks whether there is the 
#  inverse matrix for the given source matrix in the 
#  envelope.
#
#  If the inverse matrix is not available it calculates the 
#  one and puts it into the envelope and returns the result.
##
cacheSolve <- function(x) {
  m <- x$getinv()

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
