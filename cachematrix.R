## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function returns a list of following functions:
##
## set(): set the value of the matrix  
## get(): get the vallue of the matrix
## setinverse(): set the value of the inverse
## getinverse(): get the value of the inverse
##
## First, initialize inverse to NULL.
## set() function does assign an input value to x in the parent environment
## and then refresh its inverse matrix to NULL
## get() function retrieves the value of x in the parent environment
## setinverse() sets the value of inverse in the parent environment to the input value
## getinverse() gets the value of inverse in the parent environment

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL    
        }
        get <- function() x
        setinverse <- function(inv)  i<<- inv
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## First cacheSolve tries to retrive the inverse of the matrix
## and checks to see if inverse has already been calculated or not. If so,
## it returns the inverse matrix from cache. 
## Otherwise, it gets the matrix from input object, calculates its inverse 
## and then stores the value of inverse into cache.

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting inverse matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
