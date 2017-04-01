## This function creates a special "matrix" object that can cache its inverse. Specifically, it creates a list containing a function to: (1) Define a square invertible matrix (2) Obtain the value of a matrix (3) Define the inverse of a matrix (4) Obtain the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
        
        I <- NULL # Inverse of matrix
        set <- function(y) {
                x <<-y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(Inverse) I<<- Inverse
        getinverse <- function() I
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## The following function computes the inverse of the matrix created with makeCacheMatrix.R. It first checks to see if the inverse of the matrix has been computed. If so, it obtains the inverse matrix from the cache and skips the computation. Otherwise, it computes the inverse of matrix and sets the value of the inverse of the matrix in the cache via the setinverse function.

## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data for inverse of matrix")
                return(I)
        }
        
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
}