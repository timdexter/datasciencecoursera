## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## creates a matrix based on the input parameter.
## 
makeCacheMatrix <- function(x = matrix()) {

  # inv will store the cached inverse matrix
  inv <- NULL
  
  
  #getter and setter functions
    set <- function(y) {
    x <<- y
    inv <<- NULL
  }
   ##get x  
    get <- function() x
   ## set inverse function
   setinv <- function(inverse) inv <<- inverse
   ## get inverse function
   getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  
  
}


## Write a short comment describing this function

## function will return the cached copy of the matrix if it is available
## if not then the matrix inverse function is called to return the inverse matrix
## cached copy will then exist and will be accessible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  ## Check if cached matrix is present, if it is, then return it.
  if(!is.null(inv)) {
    message("Fetching cached matrix ...")
    return(inv)
  }
  ## If cached copy of not present then create it
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  return(inv)
  
  
  
}


## Sample 

# > x = rbind(c(-1/3, -1/5), c(-1/8, 1/3))

# > m = makeCacheMatrix(x)

# > cacheSolve(m)
# [,1]      [,2]
# [1,] -2.4489796 -1.469388
# [2,] -0.9183673  2.448980

# > cacheSolve(m)
# Fetching cached matrix ...
# [,1]      [,2]
# [1,] -2.4489796 -1.469388
# [2,] -0.9183673  2.448980
