## Reference: https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md 

## Assignment: Caching the Inverse of a Matrix
## Write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  ## a list containing a function to:
  ## 1. set the value of the matrix
  ## 2. get the value of the matrix
  ## 3. set the value of the inverse
  ## 4. get the value of the inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## If the inverse has already been calculated (and the matrix has not changed),
  ## then the inverse will be retrieved from the cache.
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  ## computing the inverse of a square matrix using solve()
  ## Assumption: the matrix supplied is always invertible
  ## Note: a matrix multiplied by its inverse equals the identity
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}