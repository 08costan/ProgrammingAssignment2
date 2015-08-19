## Coursera "R programming" Course Project of Week 3
## Programming Assignment 2: Lexical Scoping

## This source file defines two functions: 
## cacheSolve(): a function to calculate an inverse matrix and cache the result for faster later-on computations.
## makeCacheMatrix(): a function to create an object 'Cache Matrix' able to store the matrix and cache data.

makeCacheMatrix <- function(myMatrix = matrix()) {

    ## This function returns a special object 'Cache Matrix' with the following properties:
    ## We can store a Matrix inside this object (or access/edit it)
    ## We can store a 'Cache' Matrix Inverse inside this object (or access/edit it)
        
    # Defining the variable used to store the cache inverse matrix
    # (Similar to object attributes when defining a new class in Object-Oriented Programming)
    inverseMatrix <- NULL
    
    # Defining the 4 functions used to build the special object 'Cache Matrix'
    # (Similar to object methods when defining a new class in Object-Oriented Programming)
    setMatrix <- function(x){
        myMatrix <<- x
        inverseMatrix <<- NULL
        }
    getMatrix <- function() myMatrix
    setCache <- function(x)inverseMatrix <<- x
    getCache <- function() inverseMatrix
    
    # Returning the final 'Cache Matrix' object (i.e. a list of 4 functions)
    list(
        setMatrix=setMatrix,
        getMatrix=getMatrix,
        setCache=setCache,
        getCache=getCache
        )
}

cacheSolve <- function(myCacheMatrix, ...) {
    
    ## This function returns the inverse of a matrix. Matrix must be given as a 'Cache Matrix' object.
    ## If the inverse matrix was previously calculated, it returns the cached value,
    ## otherwise a new value is calculated with function solve() and stored in 'Cache Matrix'.
    ##
    ## Arguments:
    ##
    ## myCacheMatrix must be a 'Cache Matrix' created wih the function makeCacheMatrix().
    ## "..." are further arguments to be passed to the function solve().
    
    inverseMatrix <- myCacheMatrix$getCache()
    
    if(!is.null(inverseMatrix)) {
        # The inverse Matrix has already been calculated: no need to calculate it twice
        message("Getting cached data")
        return(inverseMatrix)
    }
    else {
        # The inverse Matrix has never been calculated before: doing the calculation now
        data <- myCacheMatrix$getMatrix()
        inverseMatrix <- solve(data, ...)
        myCacheMatrix$setCache(inverseMatrix) # Storing the result in 'Cache Matrix' if needed for a new calculation
        return(inverseMatrix)
    }
}
