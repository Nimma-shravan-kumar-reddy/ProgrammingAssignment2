## Matrix Inversion is usually costly so to reduce the cost of computation
## we can make use of cache to store it for future use rather than computing 
## it again.

## This function creates a special "matrix" object that can cache its 
##inverse.

makeCacheMatrix <- function(x = matrix()) {
  invt = NULL
  set <- function(y)
  {
    x <<-y
    invt <<- NULL
  }
  get <- function()
  {
    x
  }
  setInverse <-function(inverse)
  {
    invt <<-inverse
  }
  getInverse <- function()
  {
    invt
  }
  list(set=set,get=get,setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  
  invt <- x$getInverse()
  if(!is.null(invt))
  {
    message("getting cached data")
    return(invt)
  }
  mat <-x$get()
  invt <-solve(mat, ...)
  x$setInverse(invt)
  invt
        ## Return a matrix that is the inverse of 'x'
}
