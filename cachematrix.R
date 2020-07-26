## The first function, makeMatrix creates a special "vector", which is really a list containing a function to:

    # set the value of the matrix
    # get the value of the matrix
    # set the value of the inverse matrix
    # get the value of the inverse matrix


## Here we use the <<- operator in order to cause a search to be made through parent environments for an existing definition of the variables being assigned. For instance in set function, x is searched for within makeMatrix parent environtment.

makeMatrix <- function(x = matrix()) {
  			inv <- NULL
  			set <- function(y){
  				x <<- y
  				inv <<- NULL
  			}
  			get <- function() x
  			setInverse <- function(inverse){
  				inv <<- inverse
  			}
  			getinverse <- function() {inv}
  			list(set = set, get =get, setinverse=setinverse, getinverse = getinverse)
}


## Here we check if the object inv which has the inverse of the matrix has already been calculated(!is.null(inv)), if not, lines 35 to 38 calculate and print the inverse of the matrix passed on the x argument for cacheSolve function. 

cacheSolve <- function(x, ...) {
      
        inv <- x$getInverse()
        if(!is.null(inv)){
        		message("getting cached data")
        		return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$Setinverse(inv)
        inv
}
