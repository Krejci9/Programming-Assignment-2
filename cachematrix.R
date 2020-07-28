## The following pair of functions cache the inverse of a matrix.
## 

## The first function, makeCacheMatrix, generates a matrix object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y) {
          x <<- y
          inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Based on the function explained above, cacheSolve computes the inverse
## of the matrix that was previously generated. 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
          message("getting cached data")
          return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
}
