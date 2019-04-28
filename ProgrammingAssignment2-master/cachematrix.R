
## The function creates a new instance of the matrix with the getter and setter properties for a input matrix. 
## set() and get() allows you to access cached values,i.e. the input matrix.
## setInverse and getInverse methods provides the access to cached inverse value of the input matrix. 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The following functions are used to create a special object that stores a matrix and caches its inverse. The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
  
#set the value of the matrix

#get the value of the matrix

#set the value of the inverse

#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

