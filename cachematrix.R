## Put comments here that give an overall description of what your
## functions do

## Make cache matrix function sets and gets a matrix and also has a special matrix object to set and get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse_mat <- NULL
        set <- function(y) {
                x <<- y
                inverse_mat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_mat <<- inverse
        getinverse <- function() inverse_mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Cache solve function calculates inverse of a matrix in case the inverse is not already stored in cache  

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
        if(!is.null(inverse_mat)) {
                message("getting cached data")
                return(inverse_mat)
        }
        data <- x$get()
        inverse_mat <- solve(data)
        x$setinverse(inverse_mat)
        inverse_mat
}
