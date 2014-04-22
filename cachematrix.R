## Peer Assignment W3: Caching the Inverse of a Matrix
## 
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. 

## Please refer to usage instructions at the bottom.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	# x holds the given matrix
	# i holds the cached inverse matrix

	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() {
		x
	}
	setSolve <- function(solve) {
		i <<- solve
	}
	getSolve <- function() {
		i
	}
	list( set = set,
		get = get,
		setSolve = setSolve,
		getSolve = getSolve )
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getSolve()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	m <- x$get()
	i <- solve(m, ...)
	x$setSolve(i)
	i
}

x1 <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), c(3, 3))
x2 <- matrix(c(1, 1, 1, -1, 1, 2, 1, 1, 4), c(3, 3))

## Inverse of x1 is:
##
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1

## Inverse of x2 is:
##
##            [,1] [,2]       [,3]
## [1,]  0.3333333  1.0 -0.3333333
## [2,] -0.5000000  0.5  0.0000000
## [3,]  0.1666667 -0.5  0.3333333

## Use it as:
## 
## x1
## x <- makeCacheMatrix(x1)
## x$get() # and check it is the same as x1
## cacheSolve(x) # it really computes it!
## cacheSolve(x) # it shows the cached value!
## x <- makeCacheMatrix(x2)
## cacheSolve(x) # it really computes it!
## cacheSolve(x) # it shows the cached value!
##
## A huge matrix can then be added. This will take about 60sec for the
## first inversion, and it can be tested that it really uses the cache
##
## x3 <- matrix(runif(3000 * 3000), c(3000, 3000))
