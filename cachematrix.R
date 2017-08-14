## makeCacheMatrix will take a matrix, and store functions that allow you to store the matrix or store the inverse of the matrix, return the matrix or return the inverse of the matrix 
## cacheSolve will check to see if a inverse matrix exists for the argument x. If not, it will calculate one

## makeCacheMatrix will create a list of functions to access the matrix or inverse of the matrix, or change the matrix or change the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL   					##initilize the inv variable which will hold the inverse matrix
 	set <- function(y) {   				## internal function to set a new function
    		x <<- y
    	inv <<- NULL     				 	## setting inv back to NULL for new matrix
  }
  get <- function() x   				## internal function to get the x matrix being used
  setInverse <- function(inverse) inv <<- inverse   ## internal function to set inv to inverse matrix  
  getInverse <- function() inv            	## internal function to get inv stored
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve will check to see if a inverse matrix exists in makeCacheMatrix. If not, it will calculate a new one and set it as inv for makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()		## grab the inverse matrix from makeCacheMatrix
  
  	if(!is.null(inv)) {     ## check if null
    		message("getting cached data")
    		return(inv)       ##if not null, then return it
  }
  
  
  data <- x$get()			## grab matrix to apply inverse to from makeCacheMatrix
  
  inv <- solve(data, ...)     ## calculate inverse
  
  x$setinv(inv)			## use makeCacheMatrix internal function to set inv to inverse matrix
  	
  inv					## return inverse matrix
}
 


}
