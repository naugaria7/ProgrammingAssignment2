## The two functions makeCacheMatric and cacheSolve together use the lexical scoping feature of R
## to make computation of inverse of matrix faster if it already exists in cache. The output of makeCacheMatrix is a 
## list of four functions. The second function computes the inverse of matrix by using this list of functions
## to check if it has already been computed and if not, computes it.

## The following function takes a matrix and returns a vector which is a list of functions which can set the value of matrix,
## get the value of matrix, set the value of inverse and get the value of the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(matrix_inverse) i <<- matrix_inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function checks if the inverse of the matrix as defined by the vector returned by above function
## is already computed and present in cache. If its already present,
## it returns the value in cache. Else it computes it and stores it in cache using setmean function of the vector.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setinverse(i)
    i
}
