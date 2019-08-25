## This function will cache the inverse of a matrix


#maakeCacheMatrix is a function that creates a matrix object so that its
#inverse can be solved
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <-function() m
    list(set = set, get = get, setSolve = setSolve, getSolve= getSolve)
}


## CacheSolve is a function that computes the inverse of the special "matrix"
## made by "makeCacheMatrix" above. If it's inverse has already been determined,
## then its result will be returned from the cache rather than solved to save time

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <-x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
