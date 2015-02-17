## cachematrix.R

## Coursera: R Programming (Johns Hopkins): Homework 3, Programming Assignment 2.
##
## The two functions in cachematrix.R compute the inverse of a square matrix. 
## The first time the inverse is computed it is saved to the cache, thus speeding
##    up subsequent requests for the matrix's inverse (by avoiding 
##    re-calculation)
##
#################################################################################

##-------------------------------------------------------------------------------
## makeCacheMatrix creates a 'special matrix' object that is a list (in vector 
##    form) containing functions to:
##    1) set the value of the matrix,
##    2) get the value of the matrix,
##    3) set the value of the matrix inverse,
##    4) get the value of the matrix inverse.
## makeCacheMatrix has a return value that is a list of functions.  For example,
##    any function's definition can be seen by typing M$set (or M$get etc),
##    and any function can be called by typing M$set() (or M$get() etc).
##-------------------------------------------------------------------------------
makeCacheMatrix <- function(M = matrix()) {
  
  # initialise the inverse matrix ('InvM') as NULL (not yet computed, and 
  #   actually not a matrix yet, just a NULL value will work fine here):
  InvM <- NULL  
  
  # put a function that resets the cache into 'set'.
  # this function operates by setting 
  #   a) the input matrix ('M') set as a new vector,
  # and
  #   b) the inverse matrix ('InvM') as NULL - again, technically just a value at 
  #        this point, and not yet a matrix 
  set <- function(y) {
    M <<- y     
    InvM <<- NULL 
  }
  
  # put a function that returns the input matrix ('M') into 'get':
  get <- function() M
  
  # put a function that computes the inverse matrix ('InvM') values using solve() 
  #   into 'setinverse':
  setinverse <- function(solve) InvM <<- solve
  
  # put a function that simply returns the inverse matrix ('InvM') into 
  #   'getinverse'
  getinverse <- function() InvM
  
  # create a 'special matrix' list of the 4 functions with heading names as
  #  descriptors:
  list(set=set, 
      get=get,
      setinverse=setinverse,
      getinverse=getinverse)
}

##-------------------------------------------------------------------------------
## cacheSolve is a function that will take an input matrix, 'M', and return its
##    inverse matrix, 'InvM' (first checking if InvM is in the cache before 
##    computing)
## note: as per homework instructions, cacheSolve assumes 'M' is invertible.
##-------------------------------------------------------------------------------
cacheSolve <- function(M, ...) {
  
  # populate the inverse matrix ('InvM') from the cache by calling the getinverse
  #   function from the 'special matrix' list:
  InvM <- M$getinverse()

  # check if the inverse matrix ('InvM') was previously calculated:
  # a) if it WAS (i.e. is non-NULL), return to prompt with it.
    if(!is.null(InvM)) {  
    message("getting cached data")
    return(InvM)
    }
  # b) if it was NOT (i.e. is NULL), then need to calculate it:
  data <- M$get()             # call the get function from 'special matrix' list.
    InvM <- solve(data, ...)  # send the get function into solve() (i.e. calculate 
                              #     the inverse matrix 'InvM'!)
    M$setinverse(InvM)        # set inverse matrix in the cache for the future!
    InvM   
}
