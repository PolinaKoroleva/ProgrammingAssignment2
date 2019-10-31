## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
##Create the cache matrix by (1) setting the value of the matrix
## (2) getting the value of the matrix

makeCacheMatrix <- function(x = matrix()) { # set the object x to be matrix
        i <- NULL 
        set <- function(y){
                x <<- y 
                i <<- NULL 
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i 
        list(set=set, get = get,
             setinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.


cacheSolve <- function(x, ...) {
        m  <- i$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x $get ()
        m <- solve(data, ...)
        i$setinverse(m)
        m
}
