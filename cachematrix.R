## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    setmat <- function(y){
        x <<- y
        invx <<- NULL
    }
    getmat <- function() x
    setinv <- function(y1) invx <<- y1
    getinv <- function() invx
    list(setmatrix = setmat, getmatrix = getmat, setinverse = setinv, getinverse = getinv)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invx <- x$getinverse()
    if(!is.null(invx)){
        print("getting cached data")
        return(invx)
    }
    d <- x$getmatrix()
    invx <- solve(d)
    x$setinverse(invx)
    invx
}
