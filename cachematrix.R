## The integration of these 2 functions enables finding the invertion of a matrix
## The combination of the functions enables checking if the invertion of the
## matrix has already been calculated. If it had been it will return the 
## calculated inversion, if not it will calculate it.

## This function creates a list of 4 functions that return instances regarding 
## the inversion of the matrix:
# set: gets a matrix and makes it a global variable matrix
# get: returns the matrix value which was set using set
# setInverse: finds the inverse matrix of the given matrix and sets it as a 
#             global variable.
# getInverse< returns if set the inverse, if not set a null.

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) { #This function sets a new given matrix
      x <<- y
      I <<- NULL
    }
    get <- function() x #This function returns the value of a set matrix
    setInverse <- function(Inverse) I <<- Inverse #Sets global inverse
    getInverse <- function() I   #gets global inverse
    list(set = set, get = get,  
         setInverse = setInverse,
         getInverse = getInverse)   #The list of functions that is returned

}


## A function that uses the list of functions and the global matrix created by
## makeCacheMatrix to calculate the inverse of the matrix. If it has been set
## already it will print a message saying it is a saved argument and will print 
## it. If it has not been calculated the function will calculate the inverse and
## store it as a global variable.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
    I <- x$getInverse()
    if(!is.null(I)) {    #returns an inverse that was set in global rmvpierment
      message("getting cached data")
      return(I)
    }
    data <- x$get()  #If there is no set inverse, calculates the inverse
    I <- solve(data, ...)
    x$setInverse(I)
    I

}
