## Function makeCacheMatrix() and function cacheSolve() creat a inverse matrix
## and cache it for the downstream code to use.

## makeCacheMatrix() creates a R object that stores a matrix and its
## inverse. 

makeCacheMatrix <- function(x = matrix()) {
        invr = matrix()
        set <- function(y) {
                x <<- y
                invr = matrix()
        }
        get <- function() x
        setinvr <- function(inverMat) invr <<- inverMat
        getinvr <- function() invr
        list (set=set, get=get, setinvr=setinvr, getinvr=getinvr)
}


## cacheSolve() retrieve the inverse of the input matrix in makeCacheMatrix()
## when cache is not empty. When it is empty, it will caculate the inverse.

cacheSolve <- function(x, ...) {
        invr <- x$getinvr()
        if(!all(is.na(invr))){
                message("getting cached data")
                return(invr)
        }
        data <- x$get()
        invr <- solve(data, ...)
        x$setinvr(invr)
        invr    ## Return a matrix that is the inverse of 'x'
}
