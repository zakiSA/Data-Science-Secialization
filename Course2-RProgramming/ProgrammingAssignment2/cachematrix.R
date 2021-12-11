## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special matrix that can cache its inverse. It can
## 1) set the value of the matrix
##2) get the value of the matrix
##3) set the value of the inverse
##$) get the value of the inverse

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


## This function computes the inverse of the special matrix returned by makeCacheMatrix function. 
## If the inverse has already been computed then the value is retrieved from the cache as indicated by a message

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
