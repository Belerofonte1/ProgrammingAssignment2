# Function to create a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cached inverse as NULL
  inv <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset cached inverse when the matrix is updated
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  # Return a list of all the functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Try to get the cached inverse
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse of the matrix
  data <- x$get()
  
  # Check if the matrix is invertible
  if (det(data) == 0) {
    stop("Matrix is singular and cannot be inverted")
  }
  
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}