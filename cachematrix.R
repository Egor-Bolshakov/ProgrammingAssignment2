## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	invset <- function(inverse) inv <<- inverse
	invget <- function() inv
	list(set = set, get = get, invset = invset, invget = invget)
}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$invget()
	if(!is.null(inv)) {
		message("inverse from the cache")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$invset(inv)
	inv
}
