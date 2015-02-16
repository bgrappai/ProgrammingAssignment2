
## makeCacheMatrix creates a special matrix object that can cache its inverse
## and cacheSolve computes the inverse of the special matrix returned by
## makeCacheMatrix function. If the inverse has already been calculated then
## cacheSolve will retrieve the inverse from the cache.

## makeCacheMatrix function : create a matrix object and cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function(y) {
                x <<- y
                mat_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mat_inv <<- inverse
        getinverse <- function() mat_inv
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates inverse of matrix and if already 
## calculated then returns from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getinverse()
        if (!is.null(mat_inv)) {
                message("getting the cached matrix")
                return(mat_inv)
        }
        data <- x$get()
        mat_inv <- inverse(data, ...)
        x$setinverse(mat_inv)
        mat_inv
}
