## This is the second programming assigment of the R Programming course - Date 2016/10/30
## The overall idea of the following pair of functions is to cache the inverse of a matrix, because of the heavy calculation needed to get it.
## It is achieved by creating a special list containing four functions allowing to set and get the matrix itself as well as its inverse


## This function creates the special list containing four functions allowing to set and get the matrix itself as well as its stored inverse

makeCacheMatrix <- function(x = matrix()) {
   # when this function is used to make a cache matrix list, the matrix inverse is initialized as NULL
   Inverse <- NULL
   
   # if the matrix x is set a new value, then the Inverse is also set to NULL (previous Inverse calculations are not correct for new matrix x)
   set <- function(y) {
      x <<- y
      Inverse <<- NULL
   }
   
   # the "get" funtcion allows to just get our matrix x back from our special list that we're creating with this function  
   get <- function() x
   
   # "setInverse" will replace the stored Inverse matrix
   setInverse <- function(InverseFromCalculation) Inverse <<- InverseFromCalculation
   
   # "getInverse" will allow us to get the stored inverse matrix back from our sepcial list
   getInverse <- function() Inverse
   
   # return our special list
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## "cacheSolve" is the partner function to "makeCacheMatrix"; it actually uses the functions provided by makeCacheMatrix to set the Inverse of matrix x or to get the stored one if available
## Please note: Matrix must be a square matrix and invertible 

cacheSolve <- function(x, ...) {
   # using getInverse to pull the stored Inverse
   Inverse <- x$getInverse()
   
   # if its not NULL (i.e. already set to a non NULL value, cf. makeCacheMatrix) then return the stored value
   if(!is.null(Inverse)) {
      message("getting cached data")
      return(Inverse)
   }
   
   # if it is NULL calculate the inverse matrix (1. get matrix  2. calculate actual inverse  3. set the value of the inverse in input special list)
   inputMatrix <- x$get()
   Inverse <- solve(inputMatrix, diag(nrow(inputMatrix)), ...)
   x$setInverse(Inverse)
   
   return(Inverse)
}


## a short test run
set.seed(1)
Test_Matrix <- matrix(rbinom(9,10,0.5),3)

Test_CacheMatrix <- makeCacheMatrix(Test_Matrix)
cacheSolve(Test_CacheMatrix)

