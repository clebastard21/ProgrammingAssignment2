## ASSIGNMENT: Write a pair of functions that cache the inverse of a matrix

## FUNCTION: makeCacheMatrix
## DESCRIPTION: Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # Initializing it as an object within the makeCacheMatrix environment
  
  # Method: setmatrix
  # Descrption: Set the matrix
  # @ param y - matrix value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Method: getmatrix
  # Descrption: Get the matrix
  get <- function() x
  # Method: setinverse
  # Descrption: Invert the matrix
  # @param solve - value of inversed matrix
  setinverse <- function(inverse) m <<- inverse
  # Method: getinverse
  # Descrption: Get the inverted matrix  
  getinverse <- function() m

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## FUNCTION: cacheSolve
## DESCRIPTION: Invert the matrix and cache it. If the matrix has been cached, it retrieves the inverted matrix.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinverse() # Get the matrix within cacheSolve environment
  if(!is.null(m)) { # Check to see if the matrix has been calculated
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}