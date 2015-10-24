# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x=matrix())
{
  # Define the variable matrix1.  
  matrix1 <- NULL
  
  # set function is used to set values of x.
  set <- function(y)
  {
    x <<- y
    matrix1 <<- NULL
  }
  
  # get function is used to search for values of x.
  get <- function() x
  
  # setinverse is used to set the values of the inverse matrix.
  setinverse <- function(solve) matrix1 <<- solve
  
  # getinverse is used to get the values of the inverse matrix.
  getinverse <- function() matrix1
  
  # list function is used to list down the existing sub-functions under the 'makeCacheMatrix' function.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special matrix returned by makeCacheMatrix.
cacheSolve <- function(x, ...)
{
  # Get the inverse matrix calculated previously.
  matrix1 <- x$getinverse()
  
  # Check whether the calculation done is successful.
  # If it is not null, it will only return the values for matrix1.
  if(!is.null(matrix1))
  {
    message("getting cached data")
    return(matrix1)
  }
  
  # If it is null, proceed with the calculation steps below.
  data <- x$get()
  matrix1 <- solve(data, ...)
  x$setinverse(matrix1)
  matrix1
}