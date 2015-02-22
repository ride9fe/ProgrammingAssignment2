## The following 2 functions cache the inverse of a matrix.
##
## (1) makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y  # what is entered in makeCacheMatrix argument is now set in X
    iv <<- NULL  # clears the iv result because a new matrix is set!
  }
  get <- function() x  # get simply is equals to the matrix x
  setinv <- function(solve) iv <<- solve # iv becomes the argument passed into setinv
  getinv <- function() iv
  list(set = set, # 4 elements are created in a list: set, get, setinv, getinv as a result of calling makeCacheMatrix
       get = get,
       setinv = setinv,
       getinv = getinv) 

}


## (2) cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
##
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  iv <- x$getinv() # get iv from x (object returned from makeCacheMatrix)
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv) # if iv is NOT null, it is cached and returned, function stops here
  }
  data <- x$get() #if iv is null, make data = the matrix and pass it into solve function to get inverse
  iv <- solve(data, ...)
  x$setinv(iv) #invoke this so that iv is now the inverse of x
  iv ## Return a matrix that is the inverse of 'x'
}
