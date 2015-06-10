## Solution to "Program Assignment 2" of "R Programming" course
## imparted by Johns Hopkins University through Coursera.
## This R file contains two functions: makeCacheMatrix & cacheSolve


## makeCacheMatrix stores the value of a matrix and its inverse.
# It creates a list with 4 member functions: set, get, setInv and getInv.
# The <<- assignment operator protects the internal variables from the
# upper environment.
# It is assumed that parameter x is an invertible matrix.

makeCacheMatrix <- function(x = matrix()) { # Parameter x is the matrix
  xinv <- NULL # xinv in the inverse of matrix x
  set <- function(y) {
    x <<- y
    xinv <<- NULL # m is inversed by cacheSolve when needed.
  }
  get <- function() x # return the input matrix
  setInv <- function(inv) xinv <<- inv # set the inversed matrix
  getInv <- function() xinv # return the inversed matrix
  # Returns a list that contains these functions
  # Usage:
  # cache <- makeCacheMatrix(matrix)
  # cache$set(matrix) # sets a new matrix & sets the inverse null.
  # cache$get # returns the last setted matrix
  # cache$setInv # sets the inversed matrix
  # cache$getInv # returns the inversed matrix or null if not calculated
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve calculates the inverse matrix and stores it, 
# in an instance of makeCacheMatrix, for subsecuent calls.
# Parameter x is the four-functions list returned by makeCacheMatrix
cacheSolve <- function(x, ...) { 
  inv <- x$getInv() # gets the inverse of matrix x
  # it will be null if uncalculated, remember the line: xinv <<- NULL
  # in the makeCacheMatrix$set method.
  if(!is.null(inv)) { # if the inversion result is there
    message("getting cached inverse matrix")
    return(inv) # returns the previously calculated inversion
  }
  message("Calculating inverse matrix")
  data <- x$get() # if not, we do x$get to get the matrix object
  inv <- solve(data) # Calculates the inverse matrix
  x$setInv(inv) # Stores the inverse matrix
  return(inv) # returns the recently calculated inverse matrix
}

## Tests
# generate randomly a square, invertible matrix
matrix <- matrix(rexp(16), 4)
# generate the makeCacheMatrix object with this matrix
cacheMatrix <- makeCacheMatrix(matrix)
# from now on calculate or retrieve calculated inversion using the cacheSolve function

inverse <- cacheSolve(cacheMatrix) # First time the inverse is calcultated
inverse <- cacheSolve(cacheMatrix) # Subsequent calls with will return the cached inverse.
inverse <- cacheSolve(cacheMatrix)
inverse <- cacheSolve(cacheMatrix)
inverse <- cacheSolve(cacheMatrix)
