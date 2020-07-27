## This function creates a special "matrix" object that can cache its inverse



makeCacheMatrix <- function(a = matrix()) {
        inva <- NULL
	set <- function(x){
		a <<- x
		inva <<- NULL
	}
	get <- function() a
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inva
	list(set = set, get = get,
		setInverse=setInverse,
		getInverse=getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.


cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a'
        inv <- a$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inva)
	}
	data <- a$get()
	inva <- solve(data, ...)
	a$setInverse(inva)
	inva

}
