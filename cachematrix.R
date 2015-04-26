## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inversion <- NULL
  originalMatrix <- x ## used to determine if matrix has been changed
  
  ## return matrix
  get <- function() x
  
  ## set matrix
  set <- function(y) {
    x <<- y
    if (!identical(y,originalMatrix)) {
      message("new matrix assined. Inversion is reset to NULL")
      inversion <<- NULL
    } else {
      message("new matrix is identical to *original* (may or may not be the previous) matrix")
      message("Inversion matrix is NOT reset to null")
    }
  }
  
  ## set matrix's inversion
  setInversion <- function(matrix) inversion <<- matrix
  
  ## return matrix's inversion
  getInversion <- function() inversion
  
  list(set = set, 
       get = get, 
       getInversion = getInversion, 
       setInversion = setInversion
       )  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##if inversion is NOT NULL, return cached inversion
  if (!is.null(x$getInversion())) {
    message("returning cached inversion")
    return(x$getInversion())
  }
  ##otherwise compute inversion and return it
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInversion(inv)
  inv 
}
