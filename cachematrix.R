## Assignment: Caching the Inverse of a Matrix
## first function makeCacheMatrix creates a special "matrix" 
##object that can cache its inverse

## contains function to set the matrix and get the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
}
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}

## cacheSolve computes the inverse of makeCacheMatrix
## cachesolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if (!is.null(m)) {
    message("getting cached data")
    retrun(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
## Setting up matrix for test 
##matrix(2:5, 2, 2)
## retriving matrix by a$get()
      [,1] [,2]
[1,]    2    4
[2,]    3    5
## test the inverse cache by cacheSolve(a)
      [,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1
