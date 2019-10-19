## These functions create a special "matrix" object that can cache its inverse,
## then compute the inverse of the special "matrix. If it has already been 
## calculated then it retrieves from the cache.

## This first function creates a special "matrix", which is really a list 
##containing a function to:
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { ## takes input of matrix x
  I <- NULL ## defines I
  get <- function() x ## function that gets the value of the matrix
  setinverse <- function(solve) I <<- solve ## function that sets inverse
  getinverse <- function() I ## Returns the inverse of the matrix
  list(get = get, ## Creates a list with the functions above
       setinverse = setinverse,
       getinverse = getinverse)
}

## This second function computes the inverse of the special "matrix" returned by
## function above. If the inverse has already been calculated (and the matrix has
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
  I <- x$getinverse() ## get inverse of special "matrix" from previous function
  if(!is.null(I)) { ## first checks if this inverse has already been calculated 
    message("getting cached data")
    return(I) ## Returns inverse of matrix inputted into first function from cache
  }
  data <- x$get() ## gets the matrix inputted into first function
  I <- solve(data, ...) ## calculate the inverse of this matrix
  x$setinverse(I) ## puts the calculation of the matrix into the special "matrix"
  ## from first function
  I ## Return a matrix that is the inverse of 'x'
}