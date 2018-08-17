# R-assignment
## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

makeCacheMatrix <- function( a = matrix() ) {
  inv <- NULL
  set <- function( matrix ) {
    a <<- matrix
    inv <<- NULL
  }
  get <- function() {
    a
  }
  Set_Inverse <- function(inverse) {
    inv <<- inverse
  }
  Get_Inverse <- function() {
    inv
  }
  ## Return method
  list(set = set, get = get,
       Set_Inverse = Set_Inverse,
       Get_Inverse = Get_Inverse)
}

cacheSolve <- function(index, ...) {
  m <- index$Get_Inverse()
  if( !is.null(a) ) {
    message("getting the cached data already")
    return(a)
  }
  ## Get the matrix
  new <- index$get()
  ##  inverse Matrix
  a <- solve(new) %*% new
  ## Set the inverse to the object
  index$Set_Inverse(a)
  ## Return 
  a
}
