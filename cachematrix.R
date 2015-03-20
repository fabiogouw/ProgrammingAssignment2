## This function creates an object that can cache
## information within itself. In the example, this
## information will be the inverse of the matix

makeCacheMatrix <- function(x = matrix()) {
    ## This variable will hold the cached value
    cacheMatrix <- NULL
    
    list(
        set = function(newMatrix) {
            ## reset the object
            x <<- newMatrix
            cacheMatrix <<- NULL
        },
        get = function() {
            x
        },
        setCachedInverse = function(inverse) {
            cacheMatrix <<- inverse
        },
        getCachedInverse = function() {
            cacheMatrix
        })
}


## This function is responsible for calculating
## the inverse of matrix x. However, if it has
## already been calculated, this function retrieves
## its value from the cache.

cacheSolve <- function(x, ...) {
    ## Try to get the cached value
    theMatrix <- x$getCachedInverse()
    if(is.null(theMatrix)) {
        ## There's no cached value, so we have to calculate it
        message("calculating the inverse of the matrix..")
        currentMatrix <- x$get()    ## Get the current matrix from the object
        theMatrix <- solve(currentMatrix, ...)   ## calculation happens here
        x$setCachedInverse(m)     ## Saving it in the cache for the next generations
    }
    else {
        message("there's something in the cache...")
    }
    theMatrix
}

## Tests
## m <- matrix(c(-1, -2, 1, 1), 2,2)
## x <- makeCacheMatrix(m)
## x$get()
## inv <- cacheSolve(x)
## inv