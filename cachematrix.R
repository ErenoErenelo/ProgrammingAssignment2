## The following pair of functions cache the inverse of a matrix.
## The first function, `makeCacheMatrix`, creates a special "matrix" object that can cache its inverse.
## The second function, `cacheSolve`, computes the inverse of the special "matrix" returned by `makeCacheMatrix`.
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` retrieves the inverse from the cache.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse property when the matrix is set
  }
  get <- function() x  # Return the matrix
  setInverse <- function(inverse) inv <<- inverse  # Set the inverse property
  getInverse <- function() inv  # Return the inverse property
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  # Return a list of the methods
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  mat <- x$get()  # Get the matrix from the object
  inv <- solve(mat, ...)  # Calculate the inverse
  x$setInverse(inv)  # Set the inverse to the object
  inv  # Return the inverse
}
