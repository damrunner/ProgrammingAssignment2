## Two functions [makeCacheMatrix and cacheSolve] that will compute the 
## inverse of a matrix and cache the result to save on repeated compuations.

## makeVector creates a special "matrix", containing a function to:
## 1) set the matrix values
## 2) get the values of the matrix
## 3) set the values of the inverse
## 4) get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created with the above function. 
## First checks to see if the inverse has already been calculated. If so, 
## it gets the mean from the cache and skips the computation. Otherwise, it 
## calculates the mean of the data and sets the value of the mean in the cache
## via the setinverse function.

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