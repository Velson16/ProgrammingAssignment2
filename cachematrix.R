## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly. Following pair of functions "makeCacheMatrix" and "cacheSolve" are used to create special object that 
## stores a matrix, computes and also caches its inverse.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set<-function(y){
    x <<- y
    invmat <- NULL
  }
  get<-function(){
    x
  }
  setmatinv <- function(invervse_matrix) {
    invmat <<- invervse_matrix
  }
    getmatinv <- function() {
    invmat
  }
  list(set = set, get = get,
       setmatinv = setmatinv, 
       getmatinv = getmatinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmat <- x$getmatinv()
  if(!is.null(invmat)) {
    message ("getting cached data")
    return(invmat)
  }
  mat_input <- x$get()
  invmat <- solve(mat_input,...)
  x$setmatinv(invmat)
  invmat
}
