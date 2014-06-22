## These functions 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL ##initialising the default value for the inverted matrix
        
        ##initialising the value of the matrix that has to be inverted
        set <- function(newMatrix) {
                x <<- newMatrix
                inverse <<- NULL
        }
        
        ##this method returns the original matix
        get <- function() x
        
        ##setting the value for the inverted matrix
        setInvMatrix <- function(invMatrx) {
		inverse <<- invMatrix
	}
        
        ##this method returns the inverted matrix
        getInvMatrix <- function() inverse
        
        ##listing all the methods available to this function object
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmean()
        
        if(!is.null(m)) {
                message("Getting cached matrix.")
                return(m)
        }
        
        data <- x$get()
        
        m <- mean(data, ...)
        
        x$setmean(m)
        
        m
        
}
