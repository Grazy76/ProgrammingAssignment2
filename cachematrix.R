##This function creates a special matrix object that can cache its inverse
##In order to test it (if you really insist (:-)), please, make sure to 
##call the parameters via subsetting a list, ex. varname@get(), just like
##the hint for programming Assignment 2
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmtx <- function(solve) m <<- solve
        getinvmtx <- function() m
        list(set = set, get = get,
             setinvmtx = setinvmtx,
             getinvmtx = getinvmtx)
}
##this function calculates the inverse of the matrix of the function above 
##either by retrieving the existent one or calculating a new one if a brand
##new matrix is provided by function above with the new matrix
cacheSolve <- function(x, ...) {
        m <- x$getinvmtx()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmtx(m)
        m
}