## makeCacheMatrix  - Matrix "class" that includes an inverse variable that is cached after 
##                    its first call.  Matrix data is stored in 'x' and inverse stored in 'i'

##

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve  - Using a CacheMatrix "class" defined above calculate the Inverse of 
##               the passed Matrix data is returned in 'x$get()' and inverse either 
##               stored if not cached with x$setinv() or returned if cached meaning 
##               x$getinv() is not null.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
