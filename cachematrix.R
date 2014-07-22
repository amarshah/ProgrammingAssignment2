## This code provide a way to cache the inverse of a matrix

## This function creates a matrix object which can cache the matrix inverse
## The input is assumed to be an invertible matrix i.e. square and +ve definite

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                  x <<- y
                  inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function checks whether or not the matrix object has a cached inverse
## If it does not, it computes and stores one

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinv()
          if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)          
          }
          matrix <- x$get()
          inv <- solve(matrix)
          x$setinv(inv)
          inv  
}
