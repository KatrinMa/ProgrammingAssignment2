## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix returns a list of 4 functions:
## - set and setinverse are setter functions for x and m_inverse respectively
## - get and getinverse are getter functions for x and m_inverse respectively

## A call of makeCacheMatrix will define the 4 functions and simultanously reserve 
## space in memory for caching the inverse of a matrix
## The result of makeCacheMatrix has to be assigned to an object, e.g. avector.
##
## By avector$set(matrix1) the matrix1 is set into the reserve cache space, any
## formerly cached inverse will be deleted.
##
## cacheSolve requires an input argument of type makeVector(), e.g. avector.
## If there is a cached inverse, cacheSolve will return it.
## If there is no cached inverse, cacheSolve will calculated it, cache it in the
## reserved space and then return the inverse
## E.g. cacheSolve(avector) will return the inverse of matrix1


## Write a short comment describing this function
## makeCacheMatrix defines the setter and getter functions and reserves 
## space in memory for caching.
makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m_inverse <<- inverse
  getinverse <- function() m_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
## cacheSolve checks whether there is a cached inverse. If so, it will be return
## If not, cacheSolve calculates the inverse and then cache and return it. 
cacheSolve <- function(x, ...) {
  m_inverse <- x$getinverse()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  data <- x$get()
  m_inverse <- solve(data, ...)
  x$setinverse(m_inverse)
  m_inverse
}
