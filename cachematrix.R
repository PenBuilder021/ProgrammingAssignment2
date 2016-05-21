## The following functions 'makeCacheMatrix' and 'cacheSolve' are used to 
# cache the inverse of a matrix.

## 'makeCacheMatrix' creates a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## 'cacheSolve'  returns the inverse of the matrix. It first checks
# if the inverse has already been computed.  If so, it gets the result and 
# skips the computation.  If not, it computes the inverse, sets the value in 
# the cache via setiverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
    }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}

