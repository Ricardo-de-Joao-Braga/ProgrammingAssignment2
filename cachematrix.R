## Functions check for squared matrix, cache the value of the inverse matrix
## and recalculate when a new matrix is added.
## The exercise considers that all squared matrix can have an inverse (its an 
## assumption) - which is not true. 

## makeCacheMatrix checks for squared matrix and return an error message if 
## they are not.
## After that it presents 4 functions for get values an set values.
## At the end it produces a list that is the input for the next matrix.
## In fact, cacheSolve make the calculations for the inverse matrix.
## minv is for inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
   ## checks for squared matrix.
     if ((is.matrix (x)) & (nrow(x) == ncol(x)) == TRUE) { 
        minv <- NULL
        set <- function(y) {
            ## checks again for squared matrix, because values can 
            ## be set in "set function".
            if ((is.matrix(y)) & (nrow(y) == ncol (y)) == TRUE){
                x <<- y
                minv <<- NULL    
            } else {
                print ("not squared matrix")
            }
        }
        get <- function() x
        setminv <- function(solve) minv <<- solve
        getminv <- function() minv
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
    }
    else {
        print ("not squared matrix")
    }
}


## cacheSolve makes the calculation of the inverse matrix.
## It first check if there is a valid minv (not NULL) in makeCacheMatrix.
## If there is a not NULL value, it returns the cached value.
## If there is not a value, it means that there is a new matrix (or even a new 
## different value is used - which produces an error) and the function must 
## calculate its inverse matrix.


cacheSolve <- function(x, ...) {
    minv <- x$getminv()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setminv(minv)
    minv
}
