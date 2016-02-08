## Caching the Inverse of a Matrix

## makeCacheMatrix function will do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
        x <<- y
        invert <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invert <<- inverse
        getinverse <- function() invert
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Return a matrix that is the inverse of 'x' if matrix not chnaged.
## first time o cached data
## 2nd time if marix not chnaged than return cached inverse of matrix.

cacheSolve <- function(x, ...) {
        invert <- x$getinverse()
        if(!is.null(invert)) {
        message("Matrix not changed so getting cached data.")
        return(invert)
        }
        data <- x$get()
        invert <- solve(data)
        x$setinverse(invert)
        invert
}
