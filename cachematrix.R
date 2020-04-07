## Functions for creating a special matrix that can store an inverted matrix,
## and for getting the inverted matrix as a result

## Creates a special matrix, which is really a list containing function to
## set/get the value of the matrix, set/get the value of the cached inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(y) {
        x <<- y
        c <<- NULL
    }
    get <- function() x
    setcache <- function(cache) c <<- cache
    getcache <- function() c
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)
}


## Returns an inversed matrix (takes the cached matrix if already computed)

cacheSolve <- function(x, ...) {
    c <- x$getcache()
    if(!is.null(c)) {
        message("getting cached data")
        return(c)
    }
    data <- x$get()
    c <- solve(data, ...)
    x$setcache(c)
    c
}