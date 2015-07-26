
## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## Set the matrix
  set <- function(y) {
    matrix <<- y
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function() {

    ## Returning matrix
    matrix
  }
  
  ## Set the inverse matrix
  setInverse <- function(inverse) {
    
    ## Store inverse 
    inv <<- inverse
  }
  
  ## Get the inverse matrix
  getInverse <- function() {
    
    ## Returns the inverse
    inv
  }
  
  ## Returns the list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {

  ## Get the inverse matrix of 'x'
  inv <- x$getInverse()
  
  ## Returns if the inverse has already been calculated 
  ## (i.e. if !is.null(m)==TRUE)
  if(!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
 
  ## In case the inverse matrix is not calculated yet
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse 
  invex <- solve(data) %*% data
  
  ## Store the inverse to the object
  x$setInverse(invex)
  
  ## Return the inverse matrix of 'x'
  invex 
}
