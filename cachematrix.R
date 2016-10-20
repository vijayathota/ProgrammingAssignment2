## Put comments here that give an overall description of what your
## makeCacheMatrix fuction take matrix as an input and it caches value in variables.
## Here "inversemat" variable holds inverse matrix and it is considered as cache.
## this function has 4 sub functions 
###  set() it sets the matrix which needs to be inverserd
###  get() it return matrix.
###  setinversematrix() It set the inversed matrix
###  getinversematrix() it returns the inversed matrix   
makeCacheMatrix <- function(x = matrix()) {
  
  inversemat <- NULL
  set <- function(y)
  {
    x <<- y
    inversemat <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inmat) inversemat <<- inmat
  getinversematrix <- function() inversemat
  list(set = set, get = get, setinversematrix = setinversematrix , getinversematrix = getinversematrix)
  
}
## Write a short comment describing this function
## This function takes return object of makeCacheMatrix() as input.
## It checks if if we have any inverse matrix in cache, if yes it simply return and exit.
## If cache is empty, it would take matrix from makeCacheMatrix() and then caliculate inverse by using solve()
## result of solve() would be saved in cache.

cacheSolve <- function(x, ...) {
  mat <- x$getinversematrix()
  
  if ( !is.null(mat) )
  {
    print("Inverse Matrix is taken from Cache")
    return(mat)
  }
  
  mat <- x$get()
  inversemat <- solve(mat)
  x$setinversematrix(inversemat)
  inversemat
  
  }

