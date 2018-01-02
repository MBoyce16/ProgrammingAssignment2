## Below are a set of functions that will find an inverse of a matrix, and then cache the inverted
## matrix. These functions allow the user to call the inverted matrix without having to
## recalculate the matrix inversion.
##
## To run this script, users must 
## 1) Pass a matrix through the makeCacheMatrix() function. 
##      This function acts as a wrapper for the matrix, and provides necessary functions to 
##      store, access, and invert the original matrix.
## 2) The user should then pass the output of the makeCacheMatrix() to the cacheSolve() function.
##      This function will either computes the matrix inverse or returns the previously cached 
##      matrix.


## The makeCacheMatrix takes a single matrix as an argument, performs 6 tasks:
##
## 1) Reset the value of the cached matrix to NULL
## 2) Describe the set function, which sets the value of the original and inverted matrix
## 3) Describe the get function, which calls the original matrix.
## 4) Describe the setInverse function, which sets the value of the inverted matrix
## 5) Describe the getInverse function, which calls the inverted matrix
## 6) Outputs a list containing the four functions.


makeCacheMatrix <- function(originalMatrix = matrix()) {
        invertedMatrix <- NULL
        set <- function(y){
                originalMatrix <<- y
                invertedMatrix <<- NULL
        }
        get <- function() originalMatrix
        setInverse <- function(inverse) invertedMatrix <<-inverse
        getInverse <- function() invertedMatrix
        list(get=get, 
             set=set, 
             getInverse = getInverse,
             setInverse = setInverse)
}


## The cacheSolve function takes the output of the makeCacheMatrix as its primary argument, and
## returns the inverted matrix. The inverted matrix is calculated if using the solve() function;
## however, if the matrix had been previously cached via this function, it will call the cached
## matrix instead of performing the solve() computation.
##
## The steps for this code are as follows:
## 1)The inverted matrix is set to the value define din the parent environment
## 2)The value of invertedMatrix is checked to be NULL, if it is not NULL then the inverted
##      matrix has been computed and can be pulled form the cache.
## 3)If the inverted matrix has not been computed, then invertedMatrix = NULL and the code progresses
##        to defining the variable data as the original matrix.
## 4) The original matrix is passed throug the solve() function, the output is set to 
##      invertedMatrix



cacheSolve <- function(originalMatrix,...){
        invertedMatrix <- originalMatrix$getInverse()
        if(!is.null(invertedMatrix)) {
                message("getting cached data")
                return(invertedMatrix)
        }
        data <- originalMatrix$get()
        invertedMatrix <- solve(data, ...)
        originalMatrix$setInverse(invertedMatrix)
        invertedMatrix
}
