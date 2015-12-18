## Calculates the inverse of a matrix and stores the result in cache
## to be returned next times in order to avoid the subsequent
## calculations

## Implements the cache concept, storing a matrix and its inverse values
makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Calculates the inverse of a matrix, storing the result for
## further usage if required
cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
