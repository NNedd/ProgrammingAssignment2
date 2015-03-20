## Put comments here that give an overall description of what your
## functions do:

## The following pair of functions cache the inverse of a matrix.
## These functions are necessary because finding the inverse of a 
## matrix is a costly computation.  Caching will only attempt to 
## calculate the inverse if it has not already been done, which is 
## especially useful if it is required to be done multiple times in
## a piece of code.

## Write a short comment describing this function

## The makeCacheMatrix function is basically used to set up the
## caching.  It returns a list made up of four functions that 
## sets the value of the matrix, gets the value of ## the matrix, 
## sets the value of the inverse and gets the value of the inverse.  

## makeCacheMatrix has one argument, a matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL  ##initalises the inverse to NULL
  
  ## The following piece of code sets the value of the matrix in the
  ## global environment.
  ## It also sets the inverse in the global environment to NULL
  set <- function(y) {
      x <<- y
      s <<- NULL
  }
  
  ## The get function returns the value of the matrix
  get <- function() x
  
  ## setinverse sets the value of the inverse of the matrix
  setinverse <- function(solve) s <<- solve
  
  ## getinverse returns the inverse of the matrix
  getinverse <- function() s
  
  list( set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse )
}


## Write a short comment describing this function

## The cacheSolve funtion calculates the inverse of the matrix
## created in makeCacheMatrix.  Before it does so it checks to
## see if the inverse of the matrix was already found.  If so,
## it will get in cached inverse and return it.  If not, it will set
## the new value of the cached inverse and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
 
  ## checking to see if the inverse was already cached. 
  if(!is.null(s)){
    message('getting cached data')
    return(s)
    
  }
  
  ## gets the matrix for which the inverse has to be cached.
  data <- x$get()
  
  ## calculates the inverse of the matrix
  s <- solve(data, ...)
  
  ## sets the inverse of the matrix as the new cached value.
  x$setinverse(s)
  
  s
  
}
