## Pair of functions are executed such that we can cache the inverse of a matrix
## The goal here is to save time by caching the inverse computation of matrix

## makeCacheMatix creates the special matrix object that caches its inverse
## x is the square invertible matrix. While the return is a list of functions
## i.e. set,get,setinv & getinv

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve 
	getinverse <- function() i
      list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## x is the output from makeCacheMatrix. While it returns inverse of original
## matrix to makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}