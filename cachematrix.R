##########################################################################
## Functions for cashing the inverse of the matrix to save some CPU time.#
##########################################################################

## 1. The first function from assignment that creates a cashed matrix object

makeCacheMatrix <- function(x = matrix()) {
  # assign inverse value
  inv_x <- NULL
  # set the original matrix and reset inverse
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  # get the original matrix
  get <- function() x
  # set inverse value
  set_inv_x <- function(inv_func) inv_x <<- inv_func
  # get inverse value
  get_inv_x <- function() inv_x
  
  # Computed matrix:
  
  list(set = set, get = get,
       set_inv_x = set_inv_x,
       get_inv_x = get_inv_x)
}


## 2. The second function from the assignment that computes the inverse of the object that returned by 
## makeCacheMatrix. If the inverse matrix is cashed, then the function will used cashed object. 

cacheSolve <- function(computed_matrix, ...) {
  inv_x <- computed_matrix$get_inv_x()
  if(!is.null(inv_x)) {
    message("cached data available!")
    return(inv_x)
  }
  temp <- computed_matrix$get()
  inv_x <- solve(temp, ...)
  computed_matrix$set_inv(inv_x)
  inv_x
}