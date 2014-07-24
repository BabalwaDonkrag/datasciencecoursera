## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( j = matrix() ) {
    i <- NULL
    set <- function( matrix ) {
        j <<- matrix
        i <<- NULL
    }
    
    get <- function() {j}
    
    setInverse <- function(inverse) {i <<- inverse}
    getInverse <- function() {i}
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    j <- x$getInverse()
    if( !is.null(j) ) {
        message("getting cached data")
        return(j)
    }
    data <- x$get()
    j <- solve(data)
    x$setInverse(j)
    j
}
