## There are two paired functions in the R file that are aimed to cache  
## the inverse of a matrix.

## This function will create a special vector that returns a list of functions
## that set and get the value of the matrix, as well as set and get the inverse of
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inversion) m <<- inversion
        getinverse <- function() m
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

## This function will calculate the inverse of the matrix.  If this has already 
## been calculated, then will get the inverse from the cache. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
