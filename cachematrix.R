######################################################################################
# Author:       Wayward-Economist
# Date:         6/21/14
# Github:       https://github.com/Wayward-Economist/ProgrammingAssignment2
# Description:  This script was written to satisfy the course requirements of "R Programming"
#               offered through Coursera.org. This script is the solution to programming
#               assigmnet 2.
# Notes:        This script will find and cache the inverse of a square nonsingular matrix.
#               If the matrix is not square the existence of the inverse is not garunteed.
#               In that case consider calculating the psuedo-matrix.
# Instructions: In order to run this script preform the following:
#               1) Assign the function makeCacheMatrix() to an object. x <- makeCacheMatrix()
#               2) Assign the matrix y using the function 'set'. x$set(y)
#               3) Call cacheSolve() on x. cacheSolve(x)
#               The matrix can then be extracted with the call x$getInverse().
######################################################################################

# This function makes a list. There are four elements of the list and each element is a function.
# 1) set()        - This function takes a matrix arguement and caches it.
# 2) get()        - This function takes no arguement and returns the cached matrix.
# 3) setInverse() - This function takes a matrix arguement and caches it.
# 4) getInverse() - This function takes no arguement and returns the cached inverse.
makeCacheMatrix <- function(x = numeric()) {
  
  inv <- NULL # 
  
  set <- function(y) { # This function caches the matrix from the arguement 'y'. Note that the inverse is reset to NULL
    
    mat <<- y   
    inv <<- NULL
    
  }
  
  get <- function() mat                        # This function returns the stored matrix.
  setInverse <- function(store) inv <<- store  # This funcion chaces the 'store' arguement.
  getInverse <- function() inv                 # This function returns the stored inverse.   
  
  # Set a list to include all four of the functions defined above and return it. 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


# This function takes an arguement that should be a list created with the makeCacheMatrix() function.
# This function checks to see if the cached matrix inverse exists. If it does not, the matrix inverse is 
# calculated using the solve function and then cached. 
# Note: This function only works with square non-singular matrixes. The course assignment indicates that
# we may assume the matrix is singular and square. But two checks are included for testing purposes. 
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse() # Call the cached matrix. 
  
  if(!is.null(inv)) {   # Test to see if the inverse has already been calculated. 
    message("Getting cached inverse...")
    return(inv) # Note that return() terminates the function; if the code is executed then the code below will not be run. 
  }
  
  data <- x$get() # Get the matrix out of the cache.
  
  # Check to see if the matrix has an inverse by testing its determinant. 
  if (det(data) == 0) message("Warning: The matrix is not invertible!")
  
  # Check to see if the matrix is square; print a warning if it is.
  else if (! nrow(data) == ncol(data)) message("Warning: The matrix is not square! Consider calculating the Pseudo-Matrix")
  
  else {                             # If the matrix is both invertible and square then calculate the inverse. 
    message("Calculting inverse...")
    inv <- solve(data)               # Invert the matrix using the solve function. 
    x$setInverse(inv)                # Cache the inverse using the function from the 'x' list. 
  }
  
  inv # Return the inverse.
  
}