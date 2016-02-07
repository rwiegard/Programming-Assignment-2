## The functions allow the calculation of the inverse of a matrix. 
## The scoping rules of R are used in order to avoid time-consuming recalculations. 
## The result is stored and as long as no changes are made loaded again from memory without recalculation.

## Creates a special "matrix". Setter and getter functions are provided in order to store the initial matrix and the inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(matrix_in){
        x <<- matrix_in
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_in) inverse <<- inverse_in
    getinverse <- function() inverse
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of a special "matrix" object but only if there is no inverse stored in the memory or the initial matrix has changed. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    matrix_to_inverse <- x$get()
    inverse <- solve(matrix_to_inverse)
    x$setinverse(inverse)
    inverse
}
