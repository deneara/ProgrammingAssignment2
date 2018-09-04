# This is the code I wrote for Assignment 2 for the coursera Data Specialization R call
# It's purpose is to cache an inverse matrix adn retrieve it

# This first function creates the set, get, setinvmat, and getinvmat functions
makeCacheMatrix <- function(x = matrix()) {   # makeCacheMatrix is is a function that takes a matrix as an arg 
  m <- NULL                      # m is set as an object that will be defined later
  set <- function(y) {           # set is a function the takes the arg y
    x <<- y                      # sets y as x from the parent envi
    m <<- NULL                   # resets m to NULL in the parent envi, thus clearing m (clearing the catch)
  }
  get <- function() x            # get is a function that returns x (from parent envi)
  setinvmat <- function(solve) m <<- solve     # setinvmat is a function that sets m as the inverse of a matrix in the parent envi
  getinvmat <- function() m     # getinvmat returns m from the parent envi
  list(set = set, get = get,    # assigns each function as an element within a list within the parent envi
       setinvmat = setinvmat, getinvmat = getinvmat)
}
# The is second function checks to make sure there is cached data before computing the 
# inverse of a matrix and either retrieves that data or cumputes and sets the new data in the cache
cacheSolve <- function(x, ...) {     # cacheSolve is a function that takes the arg x as well as others
  m <- x$getinvmat()                 # the function tries to retrieve the inverse of matrix x
  if (!is.null(m)) {                 # if m is not equal to NULL, then there is a value to retrieve from the cache
    message("getting cached data")   # informs the user that the value is retrieved from the cache
    return(m)                        # returns the retrieved value
  }
  data <- x$get()                    # is !is.null(m) is FALSE, then the input (x) is retrieved 
  m <- solve(data, ...)              # the inverse of the input matrix is calculated (solve) 
  x$setinvmat(m)                     # the inverse matrix is then set to the cache
  m                                  # the inverse matrix is returned
}
