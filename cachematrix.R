## Cache the inverse of a matrix

## Create matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	mat <- NULL
	set <- function(y) {
		x <<- y
		mat <<- NULL 
	}
	get <- function() x
	setinverse <- function(o.solve) mat <<- o.solve
	getinverse <- function() mat
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Compute the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calucated, and the matrix has not
## changed, the following function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if (!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
