## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment de

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
