## Creating a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y){
    x <<- y
    mat_inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) mat_inv <<- inverse
  getInverse <- function() mat_inv 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Computes the inverse returned by makeCacheMatrix. If the inverse has aldready been
## calculated and unchanged, the inverse is retrieved form the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getInverse()
  if(!is.null(mat_inv)){
    message("Retrieving the cached data")
    return(mat_inv)
  }
  matrix <- x$get()
  mat_inv <- solve(matrix,...)
  x$setInverse(mat_inv)
  mat_inv
}
