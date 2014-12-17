## Programming Assignement 2
## Author: Davor Pavisic

## Description: in this PA, the idea is to write a copule of functions that will allow
## to cache the inverse of a given matrix and use the cached results instead of recalculating
## the inverse every time.   if the matrix changes or it is the first time being created, then
## then the inverse is calcualted with the R built in "Solve".  if the matrix remains unchanged, 
## then, every time we call the matrix inverse fucntion the program will return the cached result 
## instead saving a lot of cpu time

## Instructions:
## to create a matrix, use the function makeCacheMatrix passing as argument a valid square matrix

## 1) create a square matrix
## M < matrix(sample(16),4,4)

## 2) use makeCacheMatrix to create the special matrix
## MAT <- makeCacheMatrix(M)

## 3) calculate the inverse of MAT with cacheSolve
## cacheSolve(MAT)

## 4) subsequent calls to 3) will return the cached result instead of recalculating

## 5) changing the valude of the matrix and recalling cachSolve will recalculate the inverse
## MAT <- makeCacheMatrix(M+1)
## cacheSolve(MAT)

## Asumptions:
## the arguemetn is a valid matrix: square and with an inverse.
## the size of the matrix can be handled by the computer...  i.e.  dont give a 100000000000 x 100000000000 and 
## hope to get a result



## Function name: makeCacheMatrix
## Function receives as an argument a square matrix; the matrix has an inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }
  setInv <- function(inverse) { m <<- inverse }
  getInv <- function() { m }
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## this function takes as an argument the special matrix created with makeCacheMatrix and calculates the inverse and
## caches the resulting inverse for future use
## if subsequenc calls to this function are done with the same matrix, then instead of recalculating the inverse, 
## the cached result is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else {
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    m
  }
}
