## This program computes and caches the inverse of a matrix to avoid computing it repeatedly.

## In OOP terms, this function(read Class) can be thought of as an extension to the
## matrix Class interface. It adds functionality to the object by providing
## the capability to save and retrieve data for frequently computed computations;
## in this case inversion of the matrix.

makeCacheMatrix <- function(originalMatrix = matrix()) {

        inverse <- NULL ##initialising the default value for the inverted matrix
        
        ##resetting the value of the originalMatrix and resetting the inverse
        set <- function(newMatrix) {
                originalMatrix <<- newMatrix
                inverse <<- NULL
        }
        
        ##this method returns the original matix
        get <- function() originalMatrix
        
        ##setting the value for the inverted matrix
        setInverse <- function(cachedMatrix) {
		inverse <<- cachedMatrix
	}
        
        ##this method returns the inverted matrix
        getInverse <- function() inverse
        
        ##listing all the methods available to this function object
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function takes objects of the makeCacheMatrix Class. It then tries to retrive
## cached value of the inverse of the matrix. If that value is NULL, then it tries to
## compute the inverse of the matrix, saves the value in the matrix object and then
## returns the inverted matrix.

cacheSolve <- function(x, ...) {
        
        ##getting the value from the cached matrix otherwise NULL
        matrix <- x$getInverse()
        
        ##if there was a cached matrix then returning from function with the value
        if(!is.null(matrix)) {
                message("Getting cached matrix.")
                return(matrix)
        }
        
        data <- x$get() ##getting the matrix to compute
        
        matrix <- solve(data, ...) ##computing the inverse of the matrix
        
        x$setInverse(matrix) ##caching the inverted matrix for future use
        
        matrix ##returning the matrix
        
}
