## Put comments here that give an overall description of what your
## functions do

## Below function creates a vector containing setter/getter functions
## for the matrix and its inverse value

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(value) inv <<- value
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates matrix inverse or returns cached value if it
## was already calculated and matrix wasn't changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if (!is.null(inverse)) {
        message("returning cached value")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinv(inverse)
    inverse
}
