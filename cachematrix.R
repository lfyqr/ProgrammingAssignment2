## This function is to cache the inverse of a matrix.

## This function is to
##1. Set the value of the matrix 
##2. Get the value of the matrix 
## 3. Set the value of the inverse 
## 4. Get the value of the inverse 


makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL 
 set <- function(y) { 
 x <<- y 
 inv <<- NULL 
    } 
 get <- function() x 
 
setinv <- function(inverse) inv <<- inverse 
getinv <- function() inv 
 
list(set = set, get = get, setinv = setinv, getinv = getinv) 
} 

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
 if (!is.null(inv)) { 
  message("getting cached data") 
   return(inv) 
  } 
  data <- x$get() 
  inv <- solve(data, ...) 
  
   x$setinv(inv) 
 inv 
}

