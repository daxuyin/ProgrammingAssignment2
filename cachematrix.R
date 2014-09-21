## makeCacheMatrix creates a vector of functions that can be called from outside.
## It takes the existing variable matrix, and keeps the inverse of the matrix.
## cacheSolve will check if a inverse of the matrix was calculated before
## if it does exists, it will re-use it, otherwise, calculate the inverse matrix,
## pass it to makeCacheMatrix and save the inverse matrix for reuse later.


makeCacheMatrix <- function(x = matrix()) {
  
  
  #set variable s (Inverse of a Matrix in this case) to NULL
  
  s <- NULL
  
  #set function sets x to the argument y and set s to null, 
  #basically, clear anything exists before when x$set(y) is called
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  #get returns the value of x (argument of makeCacheMatrix)
  get <- function() x
  
  #sets s in makeCacheMatrix to Inverse Matrix, called in cacheSolve function.
  setSolve <- function(v_solve) s <<- v_solve
  
  # get returns the value of S
  getSolve <- function() s
  
  #Returns of a labeled vector, with values of the functions in makeCacheMatrix
  #so these functions can be called from outside.
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #Check if precalculated inverse matrix, s, exists.
  #If TRUE, return the precalculated s
  
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  #the matrix variable
  data <- x$get()
  
  #calculate the inverse matrix of data
  s <- solve(data, ...)
  
  #set the inverse matrix value 
  x$setSolve(s)
  
  #return the inverse matrix
  s
}
