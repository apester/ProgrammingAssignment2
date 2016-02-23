## The file contains of two functions, that are used tp create a 
## special object that stores a numerical matrix and cache's it inverse
## (if it exists)


## the first function creates a "special" vector with four functions to 
## the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
               x <<- y
              inv <<- NULL
      }
      get <- function() x
      setsolve <- function(solve)  inv <<- solve
      getsolve <- function() inv
      list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## The second function cacheSolve findes the inverse matrix. I
## Input is the "vector" calculating the inverse matrix or the function returns
## cases, when the matix is not invertible
## it cheeckes first, if the inverse matrix is already in the cache
## it checkes second, if the matrix is a squared matrix
## it checkes third, if the matrix is invertible
## if these conditions are fulfilled, it calculates the inverse
## the input is the "vector" calculating the inverse matrix or returns
## cases, when the matix is not invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv<-x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  N<-x$get()
  if(!(ncol(N)==nrow(N))) {
    message("Matrix x is not a quadratic matrix")
    return(inv)
  } else {
    if(det(N)==0) {
      message("Matrix is not invertible")
      return(inv)
    } else {
      data<-x$get()
      inv<-solve(data,...)
      inv
    }
  }
}
