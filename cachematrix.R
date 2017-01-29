## This function creates a special "matrix" object that can cache its inverse.

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  im<-NULL
  set<-function(y){
    x<<-y
    im<<-NULL
  }
  get<-function()x
  setinv<-function(invm) im<<-invm
  getinv<-function()im
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  im <- x$getinv()
    if(!is.null(im)) {
      message("getting cached data")
      return(im)
    }
    message("calculating im")
    data <- x$get()
    im <- solve(data, ...)
    x$setinv(im)
    im
}
