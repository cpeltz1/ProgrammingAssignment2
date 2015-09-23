## Matrix inversion is usually a costly computation.  
## There may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly. 
## The following are a pair of functions that cache the inverse of a matrix.

## The first function (makeCacheMatrix.R) creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix<-function(x=matrix()) {
  
  inv<-NULL
  
  set<-function(y) {
    
    x<<-y
    inv<<-NULL
  }
  
  get<-function() x
  
  setInverse<-function(inverse) inv<<-inverse
  
  getInverse<-function() inv
  
  list(set=set, 
       get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}

## The second function (cacheSolve.R) computes the inverse of the special "matrix" created by 
## the function makeCacheMatrix.R.  If the inverse has already been calculated
## (and the matrix is the same matrix), it will retrieve the inverse from the cache.

cacheSolve<-function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data<-x$get()
  inv<-solve(data)
  x$setInverse(inv)
  inv
}
