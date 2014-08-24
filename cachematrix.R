## Builds a function to compute the inverse of a matrix.

## makeCacheMatrix is a function that will create a matrix 
## that will be used to "solve" for the inverse of that matrix.

## This assignment assumes that the matrix supplied will always 
## be invertible.

## makeCacheMatrix saves the matrix to variable mat.
## The inverse of the variable mat will be variable inv.
## The method is to first cache a matrix that creates a list to
## set the value of the matrix and get the value of the matrix.
## These values will be used in the cacheSolve function
## to solve the inverse of the matrix that was created.

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function (y) {
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setmatrix <- function(solve) inv <<- solve
        getmatrix <- function () inv
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve is a user function that will solve for the 
## inverse of the matrix saved in makeCacheMatrix.  The
## variables used in this function were created with the
## makeCacheMatrix user function.

cacheSolve <- function(mat = matrix(), ...) {
        inv <- mat$getmatrix()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        matrix <- mat$get()
        inv <- solve(matrix, ...)
        mat$setmatrix(inv)
        inv
}
