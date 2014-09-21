## We are going to get the inverse of a matrix. Once it is calcualted, it is stored in
## a cache until another matrix is loaded. In this case, the inverse is recalculated.

## This function creates a list of functions that get and set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                              ## Set inverse to NULL when function is called
    set <- function(y) {                     ## If we SET the matrix, it is assigned to the
        x <<- y                              ##   stored object and the inverse is deleted
        inv <<- NULL
    }
    get <- function() x                      ## This returns the stored matrix
    setinv <- function(inv) inv <<- inv      ## Sets the inverse
    getinv <- function() inv                 ## Gets the inverse
    list(set = set, get = get,               ## Creates a list of functions and returns it
         setinv = setinv,
         getinv = getinv)
}


## This function tries to get the inverse from chache, if it does not find it, it
## is calculated.

cacheSolve <- function(x, ...) {
    m <- x$getinv()                         ## Get the inverse from cache
    if(!is.null(m)) {                       ## If not null, return stored value (and exit fn)
        message("getting cached data")
        return(m)
    }                                       ## If was null,
    data <- x$get()                         ## Read the matrix into the data
    m <- solve(data, ...)                   ## Calculate inverse (see ?solve)
    x$setinv(m)                             ## Store this inverse
    m}
