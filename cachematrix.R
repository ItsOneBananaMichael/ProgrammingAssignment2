## These functions will set up a special object that stores a
## matrix and cache its inverse value, so that R does not have to
## repeat the same calculation every time.

## makeCacheMatrix sets up a special object that stores the matrix
## and cache's its inverse. This function creates a list with
## functions to: 1) set the value of the matrix, 2) get the value
## of the matrix, 3) set the value of the inverse of the matrix,
## and 4) get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will first check to see if the inverse of the matrix
## has already been calculated. If so, it will return the cached
## result. If not, it will calculate the inverse of the matrix and
## set the value of the inverse via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
