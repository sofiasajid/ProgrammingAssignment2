## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat=matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
    print(flag)
  }
  get <- function() mat
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() 
    inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve<- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  if(nrow(data)==ncol(data) && det(data)!=0){
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv}
  
  else print("inverse is not possible as determinant is zero")
    
  
}
