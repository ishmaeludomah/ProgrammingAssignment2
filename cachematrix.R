## Create function makeCacheMatrix(). This function will create  
## a special "matrix" object that will cache its inverse.
##-----------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  # Initialize a cache to store the inverse and matrix
  cache <- NULL
  
  # Setter function to set the matrix
  set <- function(new_x){
    x <<- new_x
    cache <<- NULL # Clear the cached inverse
  }
  
  # Getter function to retrieve the matrix
  get <- function() x
  
  # Function to calculate and cache the inverse
  cacheInverse <- function(){
    if (!is.null(cache)){
      message("Getting cached inverse")
      return(cache)
    }
    
    inverse <- solve(x)
    cache <<- inverse
    inverse
  }
  
  # Return a list of functions
  list(set = set, get = get, cacheInverse = cacheInverse)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
##---------------------------------------------------


cacheSolve <- function(cacheMatrix){
  if(!is.function(cacheMatrix$cacheInverse)){
    stop("cacheMatrix must be a result of makeCacheMatrix")
  }
  cacheMatrix$cacheInverse()
}

