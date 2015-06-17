##
## The two functions makeCacheMatrix and cacheSolve are used to calculate the inverse
## of a matrix and cache the result.  Useful if you have to calculate
## the inverse of a matrix multiple times -- saves resources.  Assumes the input
## matrix is invertible.
##
## TO USE: Assume you have an invertible matrix mm with your data in it. Then 
##  amm <- makeCacheMatrix(mm)  # this is to initialize, only need it once for mm
##  invmm <- cacheSolve(amm)    # call this to calculate inverse, as many times as you need to
##


## makeCacheMatrix returns a list of functions required for the cacheSolve function.
## Input is the invertible matrix.
## Call it once, first, to initialize things for the inverse calculation:
##    amm <- makeCacheMatrix(mm)
## The functions within will be used in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL     # m is the cache variable storing the inverted matrix
  set <- function(y) {    # set initializes the data for first use
    x <<- y
    m <<- NULL
  }
  get <- function() x   # get just returns the matrix data
  setInvMatr <- function(invMatr) m <<- invMatr  # caches the inverse of the matrix
  getInvMatr <- function() m   # returns the inverse of the matrix from cache
  list(set = set, get = get,
       setInvMatr = setInvMatr,
       getInvMatr = getInvMatr)

}


## cacheSolve takes the output from makeCacheMatrix (a list of functions xl) and
## returns the inverse of the matrix that was used as input for makeCacheMatrix.
## Call it as many times as necessary -- the first time the inverse will be cached, so
## that if you call it the second time, the inverse will be returned from the cache
## rather than recalculated:
##    invmm <- cacheSolve(amm)
## If the inverse is retrieved from the cache rather than calculated, an appropriate 
## message is printed to the console for information only.

cacheSolve <- function(xl, ...) {

  m <- xl$getInvMatr()  # get the cached inverse
  if(!is.null(m)) {     # return it, with infromational message
    message("getting cached data")
    return(m)
  }     # there was no cached inverse, so calculate it, cache it, and return it
  data <- xl$get()
  m <- solve(data, ...)
  xl$setInvMatr(m)    # caching the calculated inverse
  m
  
  }
