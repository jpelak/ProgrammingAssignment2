## R functions to handle lists which, given a square matrix, provide caching of the inverse

## makeCacheMatrix: creates a list with get/set and getinverse/setinverse functions for square matricies

makeCacheMatrix <- function(m = matrix()) {
    minverse <- NULL
    set <- function(y) {
        m <<- y
        minverse <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) minverse <<- inverse
    getinverse <- function() minverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: checks for cached inverse of square matrix in list
##             if found, returns cached inverse matrix
##             if not found, computes inverse of square matrix contained in list, caching result

cacheSolve <- function(m) {
    ## Return a matrix that is the inverse of 'm'
    minverse <- m$getinverse()
    if(!is.null(minverse)) {
        message("getting cached data")
        return(minverse)
    }
    data <- m$get()
    minverse <- solve(data)
    m$setinverse(minverse)
    minverse
}
