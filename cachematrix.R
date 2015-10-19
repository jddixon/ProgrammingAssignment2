## Put comments here that give an overall description of what your
## functions do

## Given a matrix x, create a pseudo-matrix with getter and setter functions.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                           # cache is initially empty
    set <- function(y) {                # setter for matrix value
        x <<- y
        m <<- NULL                      # setting mvatrix clears cache
    }
    get <- function() x                 # getter for matrix value

    setInv <- function(inv) m <<- inv   # setter for cached value
    getInv <- function()    m           # getter for cached value

    # returns list of access functions
    list(set    = set,     get    = get,
         setInv = setInv,  getInv = getInv)
}


## Return the inverse of the pseudo-matrix x. If the inverse has already
## been calculated, use the cached value.  Otherwise compute the
## inverse, cache it, and return the value calculated.

cacheSolve <- function(x, ...) {

    m <- x$getInv()
    # if cached inverse, return it
    if(!is.null(m)) {
        message('getting cached data')
        return(m)
    }

    # otherwise get the original matrix, invert it, cache it, and return it
    data <- x$get()
    m    <- solve(data, ...)
    x$setInv(m)
    m
}
