# Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than 
# computing it repeatedly. This assignment requires writing a pair of # functions that cache the inverse of a matrix. The following two 
# functions are used to cache the inverse of a matrix.

# This 'makeCacheMatrix' function creates a special "matrix" object that can cache its 
# inverse.
# The function makeCacheMatrix creates a list containing a function 
# which
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the  value for the inverse of the matrix
# 4. get the value for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	myinv <- NULL
	set <- function(y) {
		x <<- y
		myinv <<- NULL
	}
	get <- function () x
	setinverse <- function(inverse) myinv <<- inverse
	getinverse <- function() myinv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse 

}


# This 'cacheSolve' function computes the inverse of the special "matrix" returned 
# by `makeCacheMatrix` above. If the inverse has already been 
# calculated (and the matrix has not changed), then`cacheSolve` will
# retrieve the inverse from the cache. If not, then it will calculate the inverse and 
# set it to the value in the cache.

# The assumption made is that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        # This function returns a matrix that is the inverse of 'x'
	myinv <- x$getinverse()
	if(!is.null(myinv)) {
		message("getting the cached data")
		return (myinv)
	}
	data <- x$get()
	myinv <- solve(data)
	x$setinverse(myinv)
	myinv
}
