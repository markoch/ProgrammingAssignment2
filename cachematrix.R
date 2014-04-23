## These functions calculates the inverse of a matrix. I caches the inverse matrix
## to avoid to overhead to recalculate the same invert matrix.
##########################################################################################
## Sample usage:
## m<-rbind(c(1, -1/2), c(-1/2, 1))
## cm<-makeCacheMatrix(m)
## Calculate inverse and store in cache
## cacheSolve(cm)
## Use cache
## cacheSolve(cm)
## Verify result
## cacheSolve(cm) %*% cm$get()
## Reset the cached matrix
## cm$set(rbind(c(1, -1/4), c(-1/1, 4)))
## cm$get()
## Calculate inverse and store in cache
## cacheSolve(cm)
## cacheSolve(cm)
## Verify result
## cacheSolve(cm) %*% cm$get()

## Create a modified "matrix". Provides features to get, set, getsolve and setsolve
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This will calculate the inverse of a matrix 'x'. If it was calculated before
## then the cached inverse is returned.
cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached inverse")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
