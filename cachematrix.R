## The first function creates the matrix and its associated
## functions and the second one checks if the matrix 
## already exists and if it hadn't change, if so, it returns
## the memory stored inversion for it, if don't calculates
## its inversion


## Creates the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
    
}


## Checks if the matrix exists and if it's square
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    } 
    else if ((nrow(x$get()) == 2) && (ncol(x$get()) == 2)) {
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
    }
    else
    {
        message("the matrix is not square")
        x$setinverse(NULL)
    }
}
