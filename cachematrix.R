## This function creates a "special matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of  the "special matrix" defined in makeCacheMatrix
## bypassing computation for those matrix which previously computed inverse
## by displaying the old result while computing inverse for those "new" matrices
  
CacheSolve<- function(x=matrix(),...) {
    inv<-x$getInverse()  
  if(!is.null(inv)){
    message  ("getting cache data")  #check if old result is available and use it
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat)  #compute the inverse if there is no old result
  x$setInverse()
  inv

}


