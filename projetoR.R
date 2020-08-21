# Part 1
# makeCacheMAtrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  jota <- NULL
  set <- function(y){
    x <<- y
    jota <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) jota <<- inverse
  getInverse <- function() jota 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

# Part 2
# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
# changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  jota <- x$getInverse()
  if(!is.null(jota)){
    message("getting cached data")
    return(jota)
  }
  mat <- x$get()
  jota <- solve(mat,...)
  x$setInverse(jota)
  jota
}