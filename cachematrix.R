## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. The following two function cache the inverse of matrix.



## This function creates a special "matrix" object that can cache its inverse.
# 1- set, sets the value of the matrix
# 2- get, gets the value of the matrix
# 3- setinverse, sets the value of inverse of the matrix
# 4- getinverse, gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache. Assumption has been made that the
## matrix is always invertible.
# If x is a square invertible matrix then solve(x) return its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <-x$getinverse()
	if(!is.null(m)) {
		message("getting cached matrix")
		return(m)
	}
	data <-x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
