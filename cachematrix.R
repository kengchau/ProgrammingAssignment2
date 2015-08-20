## creates a special "matrixcache" and produce a list containing
## functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix set
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()){
  
  ##initialize cache to null
  matrixCache <- NULL
  
  ## set the matrix in the working environment
  set <- function(y) {
    x <<- y
    matrixCache <<- NULL
  }
  
  ## get matrix values
  get <- function() x
  
  ## set the inversed matrix values
  setInv <- function(inv) matrixCache <<- inv
  
  ## get the inversed matrix 
  getInv <- function() matrixCache
  
  ## create a list consist of the functions for the working environment
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...){
  ## Return a matrix m that is the inverse of 'x' 
  
  m <- x$getInv()
  if(!is.null(m)) {
    ## print message
    message("getting cached data")
    ## return matrix m
    m
  }
  else{
    ## get the matrix and update into matrix
    matrix <- x$get()
    ## solve the inverse for matrix and put into m
    m <- solve(matrix, ...)
    ## call the function setInv and push the cache
    x$setInv(m)
    ## return results matrix m
    m
  }
}
