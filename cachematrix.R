## First of all, I have set up a function that ultimately creates a matrix
## object, whose inverse is cached. Then the other function can check if the
## inverse of the same matrix (not changed) has already been computed. If so,
## it retrieves the output from the cache, instead of calculating it again. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) inv <<- solve
  get_inv <- function() inv
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if(!is.null(m)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}