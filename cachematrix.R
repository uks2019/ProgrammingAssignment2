##A pair of function that caches the inverse of matrix .The lexical scoping concept to handle closure of enclosing environment is handled in makecache matrix and cachsolve. 
## x and Y 

## Write a short comment describing this function
##creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse property
  inv <- NULL
  ## method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##method to get the matrix
  get <- function() x ##return the matrix
  ##method to set inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  ##method to get inverse of the matrix
  getInverse <- function() inv
  ##retrun a list of the methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
##compute the inverse of the matrix returned by makeCacheMatrix ,if the inverse
## is already calculated and matrix is not changed then cachsolve should retrieve inverse from the cache

  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    ## message is slef explanatory
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
  ##get the matrix from the object
      mat <- x$get()
      ## calculate the inverse using solve method
    inv <- solve(mat, ...)
    ## set the invere to the object
    x$setInverse(inv)
    ##return the matrix
    inv
  }
  

