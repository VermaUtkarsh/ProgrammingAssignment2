## Creating a special matrix object that can cache its inverse

# Parent function
makeCacheMatrix <- function(x = matrix()) {  #Assuming the matrix to be invertible
  mat_inv <- NULL
  set <- function(y){  #Setting the value of the matrix
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


## Computes the inverse returned by makeCacheMatrix. If the inverse has already been calculated and unchanged, the inverse is retrieved form the cache. 

# Child function
cacheSolve <- function(x, ...) {
  mat_inv <- x$getInverse()  ## Return a matrix that is the inverse of x
  # Checking if the inverse has already been calculated
  if(!is.null(mat_inv)){
    message("Retrieving the cached data")
    return(mat_inv)
  }
  matrix <- x$get()
  mat_inv <- solve(matrix,...)  # Computing the inverse
  x$setInverse(mat_inv)
  mat_inv
}
