## Put comments here that give an overall description of what your
## functions do
## This functions is to create inverse of a matrix and cache it


## Write a short comment describing this function
## makeCacheMatrix will create a list of functions
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # create a data to store the cache matrix
  mdata <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    mdata <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse
  setcacinv <- function(inverse) mdata <<- inverse
  # get the value of the inverse
  getcacinv <- function() mdata
  # To populate the list according to the new functions
  list(set = set, get = get,
       setcacinv = setcacinv,
       getcacinv = getcacinv)
}


## Write a short comment describing this function
## cacheSolve will calculate the inverse of matrix. 
##If the inverse has been calculated before then it will return the cached value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mdata <- x$getcacinv()
  ## To check if inverse has been calculated before. If yes will return the cached value
  if(!is.null(mdata)) {
    message("getting cached data")
    return(mdata)
  }
  ## To calculate the inverse
  data <- x$get()
  mdata <- solve(data, ...)
  x$setcacinv(mdata)
  mdata
}
