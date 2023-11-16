## caching the inverse of a matrix

## computes the inverse of the special matrix 

makeCacheMatrix <- function(x = matrix()) {
  #initialize matrix
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(inv){
    inverse <<- inv
  }
  getinverse <- function(){
    inverse
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## if inverse has already been calculated then the cachesolve retrieves the
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

