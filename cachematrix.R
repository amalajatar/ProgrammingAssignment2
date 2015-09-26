# 1 : makeCacheMatrix() 
# This function creates a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  
  inv = NULL
  
  # set
  set = function(y) 
  {
     
    x <<- y        # assign variable in anotherenvironment (global)
    inv <<- NULL   # assigning Null at the beginning
  }
  
  get = function() x      # get matrix
  setinv = function(inverse) inv <<- inverse         # set the inverse matrix
  getinv = function() inv                            # get the inverse matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# 2 : cacheSolve()
# This function computes the inverse of the matrix returned by makeCacheMatrix  

cacheSolve <- function(x, ...)    # x is output of makeCacheMatrix()
{
  
  
  inv = x$getinv()
  
  
  if (!is.null(inv))            # check if the inverse has already been calculated
  {
    message("Getting data from the cache")         # pick it from the cache and dont calculate again
    return(inv)
  }
  
   
  mat.data = x$get()                          # else calculate the inverse
  inv = solve(mat.data, ...)
 
  x$setinv(inv)                          # use the setinv function to set value of inverse in cache
  
  return(inv)                           # return inverse of x
}
