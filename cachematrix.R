## These functions are used to calculate the inverse function of a matrix. 
## We can cache its value so that it can be resumed without calculating it again. Or if no value has been cached, the function calculates it.
## In this way we can cache the value of the inverse matrix, so if we need it, we can resume it without computing it again, in order to save on calculation time.

## The first function, "makeCacheMatrix" creates a special "vector", a list containing a function to set and get the value of the vector,
## and to set and get the value of the inverse matrix.
   
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL 
  set <- function(y) { #set the matrix.
    x <<- y
    m <<- NULL 
  }
  get <- function() x #get the matrix
  setinverse <- function(solve) m <<- solve #set the inverse matrix.
  getinverse <- function() m #get the inverse matrix.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} 

## This second function, "cacheSolve", calculets the inverse matrix. If we have set the inverse matrix in the above function,
## cacheSolve gets the inverse matrix from the cache and skip the computation ("getinverse" function). If not, it computes the inverse function and sets it with the "setinverse" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
  m <- x$getinverse()
  if(!is.null(m)) { #if we have cached the inverse matrix.
    message("getting cached data")
    return(m) #we obtain the inverse matrix set in the above function ("makeCacheMatrix).
  }
  data <- x$get()
  m <- solve(data, ...)  #Otherwise the function compute the inverse matrix.
  x$setinverse(m) #set the inverse matrix just calculated.
  m
}
