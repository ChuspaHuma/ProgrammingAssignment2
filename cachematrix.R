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
## M <- matrix(sample(16),4,4)

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

## Tests:
## tested with matrices that can be visually inspected i.e. M[4x4] inv(M) and inv(inv(M)) should be equal to M
## for bigger matrices, they were compared with all.equal(M1,M2)... this is a good testting fuction beause it tests 
## for near equality.
## tested with 100x100 mat... no noticeable difference when calculating the inverse vs cached inverse
## tested with 1000 x 1000 mat.    noticeable difference when calculating the inverse vs cached inverse
## 


## Function name: makeCacheMatrix
## Function receives as an argument a square matrix; the matrix has an inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                     # reset to NULL every time makeChacheMatrix is called
  set <- function(y) {          # define what will 'set' do 
    x <<- y
    m <<- NULL
  }
  get <- function() { x }       # define what will 'get' do
  setInv <- function(inverse) { m <<- inverse } # calculate the inverse when called the first time 
  getInv <- function() { m }                    # return the cached value
  list(set = set, get = get,                    # define the functions that can access the object just created
       setInv = setInv,
       getInv = getInv)
}


## this function takes as an argument the special matrix created with makeCacheMatrix and calculates the inverse and
## caches the resulting inverse for future use
## if subsequenc calls to this function are done with the same matrix, then instead of recalculating the inverse, 
## the cached result is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()              # access the object x with the function defined to access the object when created
  if(!is.null(m)) {            # if not null (i.e. its not the first time calling get, then return the cached inverse)
    message("getting cached data")  # print message to alert that cached data is being returned
  }
  else {                       # if null then its the first time inverse is being called and need to caculate
    data <- x$get()            # the inverse of the matrix: put matrix into temp variable 'data'
    m <- solve(data)           # calculate the inverse with solve and store it in m
    x$setInv(m)
  }
  return(m)                    # at this point m has the correct value cached or calculated so just return m
}
