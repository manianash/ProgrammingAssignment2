############################################################
## A pair of functions that cache the inverse of a matrix ##
############################################################

## 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions where:
## setMatrix: sets the value of the matrix
## getMatrix: gets the value of the matrix
## setInverseMatrix: sets the value of the inverse of the matrix
## getInverseMatrix: gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  setMatrix <- function(y) {
    
    x <<- y
    inverseMatrix <<- NULL
    
  }
  
  getMatrix <- function() x
  setInverseMatrix <- function(inverse) inverseMatrix <<- inverse
  getInverseMatrix <- function() inverseMatrix
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
  
}


## 'cacheSolve' function calculates the inverse of the matrix returned by 'makeCacheMatrix'. 
## It first checks if the inverse has already been calculated and if so, it gets the result and skips the calculation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the mean in the cache via the 'setInverseMatrix' function.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverseMatrix()
  
  #check if the inverse has lready been calculated
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #if not, calculate the inverse
  inverse <- solve(x$getMatrix())
  x$setInverseMatrix(inverse)
  inverse
  
}

########################
## test the functions ##
########################
# 
# set.seed(1234)
# mat <- rnorm(25)
# mat1 <- matrix(mat, nrow=5, ncol=5)
# m <- makeCacheMatrix(mat1)
# cacheSolve(m)
# cacheSolve(m)
# 
# # > cacheSolve(m)
# #           [,1]      [,2]       [,3]       [,4]       [,5]
# # [1,] -1.422040  1.438988  -1.628982 -0.3694698 -0.5029832
# # [2,]  2.903350 -3.505745   3.563584 -0.1352708  0.6882113
# # [3,]  2.850793 -5.321075   5.452092  0.6602019  1.2897834
# # [4,] -2.065381  3.466124  -4.011938 -0.4977805 -0.6328853
# # [5,] -7.854210 10.098876 -12.010162 -0.8753507 -3.0556750
# # > cacheSolve(m)
# # getting cached data
# #           [,1]      [,2]       [,3]       [,4]       [,5]
# # [1,] -1.422040  1.438988  -1.628982 -0.3694698 -0.5029832
# # [2,]  2.903350 -3.505745   3.563584 -0.1352708  0.6882113
# # [3,]  2.850793 -5.321075   5.452092  0.6602019  1.2897834
# # [4,] -2.065381  3.466124  -4.011938 -0.4977805 -0.6328853
# # [5,] -7.854210 10.098876 -12.010162 -0.8753507 -3.0556750
