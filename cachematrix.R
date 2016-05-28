## The following functions create a special matrix and make 
## inverse of the matrix available in the cache.

## makeCacheMatrix creates a special matrix that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
	
		cacheMatrix <- NULL
		
		# set the matrix
		set <- function(y) {
			x <<- y
			cacheMatrix <<- NULL
		}
		
		# get the value of the matrix
		get <- function() x
		
		# invert the matrix
		setInvMatrix <- function(inverse) cacheMatrix <<- inverse
		
		# get the value of inverted matrix
		getInvMatrix <- function() cacheMatrix
		
		#define environment with 4 function
		list(set=set,get=get,setInvMatrix=setInvMatrix,
			  getInvMatrix=getInvMatrix)

}


## cacheSolve compute the inverse of the matrix returned by
## makeCacheMatrix function. It checks if the inverse of the matrix
## been calculated and its cached , if not it will create inverse of
## the matrix and stored in the cache

cacheSolve <- function(x, ...) {
		
		# check if inverse of the matrix is in cache
		cacheMatrix <- x$getInvMatrix()
		
		# if exists return inverted matrix from cache
		if(!is.null(cacheMatrix)) {
				message("getting cached data")
				return(cacheMatrix)
		}
		
		# if doesn't exist, it creates a new matrix
		getMatrix <- x$get()
		
		# calculates and store inverted matrix in cache
		cacheMatrix <- x$setInvMatrix(getMatrix)
		
        # Return an inverse of the matrix 
        cacheMatrix
}
