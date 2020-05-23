## The makeCacheMatrix and the cacheSolve functions together help to calculate the 
## inverse of a matrix. These functions prevent repeated calculations by storing an
## already computed value in the cache. This can be retrieved and displayed when the
## inversion of the same matrix is required.

## The makeCacheMatrix function basically sets and gets the values of both 
## the matrix (n) and its calculated inversion (i).

makeCacheMatrix <- function(n = matrix()) {
        i <- NULL
        set <- function(y) {
                n <<- y
                i <<- NULL
        }
        get <- function() n
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function checks if the matrix inversion (i) has already been calculated,
## otherwise it computes the inversion, sets and then returns the value.	

cacheSolve <- function(n, ...) {
        i <- n$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- n$get()
        i <- solve(data, ...)
        n$setinverse(i)
        i
}

