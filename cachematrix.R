## Functions will solve the inverse of a matrix and store it in a cache.
## When the inverse matrix is needed again, it will pull it from the cache
## instead of solving it again.

## Function that creates a list of functions that:
## 1. set a matrix value
## 2. gets the value of the matrix
## 3. sets (stores) the value of the inverse matrix
## 4. gets the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Function pulls the value of the inverse matrix
## If the value is NULL, it solves the inverse and 
## places its value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getsolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}
