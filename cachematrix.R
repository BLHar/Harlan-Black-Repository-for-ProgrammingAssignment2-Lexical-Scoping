## To cache the inverse of a matrix so that when we need it again, it can be looked up in the cache rather than recomputed

## makeCacheMatrix
## Argument must be of class matrix and a square matrix.
#1. Sets the value of the matrix in cache
#2. Gets the value of the matrix in cache
#3. Sets the value of the inverse in cache
#4. Gets the value of the inverse in cache

# Example:
# mtrx <- matrix(1:4, nrow = 2, ncol = 2)
# m <-  makeCacheMatrix(mtrx) 

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL  ## solved matrix
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setsolved <- function(solved) s <<- solved
     getsolved <- function() s
     # Note that it returns a list of the objects that are in cache.
     # Elements of this list are accesed and used by the associated cacheSolve function, which uses the $ accessor
     list(set = set, get = get,
          setsolved = setsolved,
          getsolved = getsolved)
}

## Returns from cache the inverse of a matrix that is also in cache
## Argument for cacheSolve must the list that was returned by calling makeCacheMatrix on the matrix
cacheSolve <- function(x, ...) {
     # Access list elements that are in cache
     s <- x$getsolved()
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     # Compute the inverse
     s <- solve(data, ...)
     # Put the inverse into cache
     x$setsolved(s)
     # Return the inverse
     s
}

#=============
# FOR TESTING
# mtrx <- matrix(1:4, nrow = 2, ncol = 2)
# m <-  makeCacheMatrix(mtrx) 
# mtrx
# cacheSolve(m) ## argument for cacheSolve must be of type makeCacheMatrix
