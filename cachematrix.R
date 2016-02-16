## This file defines two complementary functions: 
# One defines a "chached" matrix "class" and 
# the second defines a method to calculate 
# the inverse of objects of this class.


## Creates a "chached" matrix object. 
# This object contains the matrix itself and its inverse, in a cache, 
# if it was ever calculated with cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL;
	
	set <- function(y) {
		x <<- y;
		inverse <<- NULL;
	}
	
	get <- function(){ 
		x;
	}
	
	setInverse <- function(inv) {
		inverse <<- inv;
	}
	
	getInverse <- function() {
		inverse;
	}
	
	list(set = set, get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## Returns the matrx inverse. 
# If this function all used before, 
# and the matrix has not changed, the chached value is used.

cacheSolve <- function(x) {
	inverse <- x$getInverse();
	
	if(!is.null(inverse)) {
		message("getting cached data");
		return(inverse);
	}
	
	data <- x$get();
	
	inverse <- solve(data);
	x$setInverse(inverse);
	
	inverse;
}
