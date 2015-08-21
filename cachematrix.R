## below are two functions that are used to create a special object that stores a square invertible matrix and cache's in inverse  

## The first function, makeCacheMatrix creates a special "matrix" object, which is a list containing function to
##1 set the elements of matrix
##2 get the elements of matrix
##3 set the inverse of matrix
##4 get the inverse of matrix
## The first function makeCacheMatrix will accept a square invertible matrix (x=matrix()) as an input.

makeCacheMatrix <- function(x = matrix()) ## input a square invertible matrix as input to this function. 
{
  xinv <- NULL
  setmatrix <- function(y=matrix()) ## function which accepts an input to a new square invertible matrix which will overwrite the existing matrix entered by using lexical scoping feature of R
   {
    x <<- y
    xinv <<- NULL
   }
  getmatrix <- function() x ## returns the elements of matrix 
  setinverse <- function(inverse) xinv <<- inverse ## sets the new inverse of matrix using lexical scoping feature
  getinverse <- function() xinv ## returns the inverse of matrix 
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the matrix created with the above function. 
##However if the inverse has already been calculated and if the matrix has not changed then it will retrive the inverse from the cache

cacheSolve <- function(x, ...) 
{
  xinv <- x$getinverse() ##getting the current value of matrix inverse from the matrix object obtained from above function
  if(!is.null(xinv)) ## Check for cached value of matrix inverse 
  {
    message("getting cached data")
    return(xinv)
  }
  data <- x$getmatrix() 
  xinv <- solve(data, ...) ## to calculate the inverse
  x$setinverse(xinv)
  xinv
}
