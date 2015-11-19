## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a matrix and its functions are:
## - set() <- setting a matrix
## - get() <- return a matrix
## - setinverse() <- setting a inverse of matrix
## - getinverse() <- return a inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
    inversa <- NULL
    set <- function(y) {
      x <<- y
      inversa <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inversa <<- inverse
    getinverse <- function() inversa
    list(
          set = set, 
          get = get,
          setinverse = setinverse,
          getinverse = getinverse
        )
}


## Write a short comment describing this function
## return the inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversa <- x$getinverse()
    if(!is.null(inversa)) {
      message("getting cached matrix inverse")
      return(inversa)
    }
    data <- x$get()
    inversa <- solve(data, ...)
    x$setinverse(inversa)
    inversa
}


## example for use:
## x <- matrix(1:4, nrow = 2)
## x
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## y <- makeCacheMatrix(x)
## res <- cacheSolve(y)
## res
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5