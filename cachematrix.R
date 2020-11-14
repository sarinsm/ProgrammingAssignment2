## Assignment intends to write a pair of function which cache the inverse of a matrix. 
## Two functions are written below: makeCacheMatrix and cacheSolve

## The makeCacheMatrix function returns a special "matrix" object containing matrix and its inverse as cache

makeCacheMatrix <- function(x = matrix()) {
  set<- x
  setinv<- solve(x)
  list(set=set,setinv=setinv)
}


## The cacheSolve function retrieves the inverse from the special"matrix" created by makeCacheMatrix function if already calculated

cacheSolve <- function(x=matrix()) {
  z<-makeCacheMatrix(x)
  inv <- z$setinv
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-z$set
  inv<-solve(data)
  z$setinv(inv)
  inv
}
