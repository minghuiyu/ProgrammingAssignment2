## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  original <- NULL
  if (is.null(original)) original<<-TRUE
  isOriginal <- function() original
  
  isOriginal <- NULL
  inversion <- NULL
  identical <- TRUE
  get <- function() x
  
  set <- function(y) {
    ##if set is called, test if new assigned object is identical to the original object
    identical <<- identical(y,original)
    x <<- y
  }
  
  getInversion <- function() inversion<<-solve(x)
  ## return TRUE if newly assigned object is identical to the previous one.Default is TRUE
  ifIdentical <- function() identical
  list(set = set, 
       get = get, 
       getInversion = getInversion, 
       ifIdentical = ifIdentical
       )
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##if identical=TRUE AND inversion is NOT NULL, return cached inversion
  if (x$ifIdentical()==TRUE) {
    return(TRUE)
  }else{
    return(FALSE)
  }
  ##otherwise compute inversion and return it
  
}
