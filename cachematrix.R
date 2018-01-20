## Coursera R Programming Week 3 Assignment
## This function creates a special "matrix" object
## that can cache its inverse

## This function can:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  
  get <- function() x
  setInversa <- function(Inversa) INV <<- Inversa
  getInversa <- function() INV
  list(set = set, get = get,
       setInversa = setInversa,
       getInversa = getInversa)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix  
## If the inverse has already been calculated then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  INV <- x$getInversa()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  data <- x$get()
  INV <- solve(data, ...)
  x$setInversa(INV)
  INV
}
