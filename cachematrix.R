## Put comments here that give an overall description of what your
## functions do

## This function creates a special matri object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # creates a list containing a function to 
        # 1 set the value of the matrix
        # 2 get the value of the matrix
        # 3 set the value of the inverse of the matrix
        # 4 get the value of the inverse of the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of a square matrix returned by makeCacheMatrix function
## if the inverse is already calculated and the matrix is unchanged, retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## NOTE: function assumes matrix is invertible!
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
