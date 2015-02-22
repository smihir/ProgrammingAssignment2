# This module defines a set of functions that create the special "matrix"
# object which can cache the value of its inverse.

## makeCacheMtrix creates a 'special matrix' object from the given matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}


## cacheSolve is used to compute the inverse of the 'special matrix'
## object which is created by makeCacheMatrix, it also caches the
## inverse so that inverse is not computed again for the same matrix

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
