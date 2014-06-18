## makeCacheMatrix returns a list of 4 functions as below
#setmatrix: save the input matrix to cache
#getmatrix: retrieve matrix from cache if it is cached
#setinverse: save the inversion of the matrix to cache
#getinverse: retrieve the inversion of the amtrix from cache 
makeCacheMatrix <- function(x = matrix()) {
  inv<-matrix(data=NA)
  setmatrix<-function(y=matrix()){
    x<<-y
  }
  getmatrix<-function() x
  setinverse<-function(inverse)
    inv<<-inverse
  getinverse<-function() inv
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve takes the input of a list
#when run, it first tries to retrieve the inverse from cache using "getinver()" fuction
#if the first element retrieved is NA,indicating no inversion has been saved, 
#it will calculate the inversion by calling "solve" function and save the result to cache using "setinverse()" fuction
# next time the same matrix is passed to cacheSolve(), it will retrieve the saved inversion from cache

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.na(inv[1,1])){
    message("getting cached inversion")
    return (inv)
  } else{
    inv<-solve(x$getmatrix())
    x$setinverse(inv)
    inv
  }
}

m<-makeCacheMatrix(matrix(c(1,3,5,7,9,11,12,14,15),3,3))
cacheSolve(m)

cacheSolve(m)
