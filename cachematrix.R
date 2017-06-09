# The below code writes a pair of functions that cache the inverse of a matrix. 

# makeCacheMatrix below is set up to create a list containing a function to do the following... 
# Firstly, 'set' the value of the matrix; secondly retrieve ('get') the value of the matrix,
# thirdly 'set' the value of the inverse of the matrix; and finally retrieve ('get') the value of
# the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve below returns the inverse of the matrix by first checking if the 
# inverse has already been computed, and if so, it gets the result - the end. 
# If the inverse has not been computed, it computes the inverse and sets the 
# value in the cache as the 'setinverse' function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}