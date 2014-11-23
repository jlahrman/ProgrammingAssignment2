## Put comments here that give an overall description of what your
## functions do

# Well that's a good question. I do not have much of a programming background
# and had to get some help to interpret exactly what was going on in the
# example functions. As best as I can tell, makeCacheMatrix essentially sets
# up a list that stores inverted matrices that can then be called by cacheSolve.
# cacheSolve calculates the inverse of a matrix and returns it the first time
# the function is run on a matrix, and thereafter it can find it in the cache.


## Write a short comment describing this function

#This function runs four functions on the matrix entered:
# set resets m to NULL each time the function starts
# get does nothing but hold the matrix that was entered
# setmatrix is used to hold the inverse of the matrix the first time cacheSolve is run
# get matrix holds the inverse of the matrix and return it to cacheSolve when
# the function is run on a matrix subsequently

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get = function() x
  setmatrix = function(inv_mat) m <<- inv_mat
  getmatrix = function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function

# m is set to the getmatrix function of makeCacheMatrix. If there is a value
# for getmatrix (which would be the previously calculated inverse of x), that
# is what is returned.
# If there is a NULL value of getmatrix in makeCacheMatrix, cacheSolve calculates
# the inverse of x, passes it to setmatrix, and returns it.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m = x$getmatrix()
  if(!is.null(m)) {
  message("getting cached data")
  return(m)
  }
  matrix = x$get()
  m = solve(matrix, ...)
  x$setmatrix(m)
  m
}