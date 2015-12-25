## these two function cache the inverse of an input matrix
## 

## the first function create an object contained the input matrix, 
## the object should be used for the input of the second function

makeCacheMatrix <- function(x = matrix()) {
    ivM <- NULL
    set <- function(y) {
        x <<- y
        ivM <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse)   ivM <<- inverse
    getInverse <- function() ivM
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## the second function returns the inverse of the input matrix
## if the inverse matrix have been computed already, it caches the inverse matrix
## if the inverse matrix have not been computed, it computes the inverse matrix
cacheSolve <- function(x, ...) {
    ivM <- x$getInverse()
    if(!is.null(ivM)) {
        message("getting cached inverse of matrix")
        return(ivM)
    }
    data <- x$get()
    ivM <- solve(data, ...)
    x$setInverse(ivM)
    ivM
}
