## These two functions computes inverse of invertible matrix 
## by taking a matrix as an input from the user

## makeCacheMatrix function accepts a matrix as an input from the user 
##and set the value of cache to inverse of matrix

makeCacheMatrix <- function(mat=matrix()) {
  inv <- NULL
  set <- function(y) {                                    # set a value of matrix
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat                                  #  Get a value of matrix
  setinverse <- function(inverse) inv <<- inverse        #  set a cache to inverse of matrix  
  getinverse <- function()                               #  get a inverse matrix
    inv
  
  list(set = set, get = get,                              # function returns a list so that user can set and get a matrix and so its inverse dynamically
       setinverse = setinverse,
       getinverse = getinverse)
}


##  cacheSolve function cehecks the cache for inverse or to calculate the inverse after validating the matrix for inverse calculation

cacheSolve<- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  if(nrow(data)==ncol(data) && det(data)!=0){   # checks for the square matrix and "0" determinant(valid input)
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv}
  
  else print("inverse is not possible as determinant is zero or mattrix is not square matrix")
    
  
}
