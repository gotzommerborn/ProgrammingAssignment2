# Little modifications are needed regarding the vector example of the assignment.
# changed some terms: mean --> MyInverse, declare matrix and use R function Solve instead of Mean.
# used https://class.coursera.org/rprog-008/forum/thread?thread_id=174: #"I will show you the output 
# from the sample functions to illustrate their behaviour. From the output you should be able to discern 
# the logic within the R code. The functions we are expected to implement to handle invertible matrices 
#follows the exact same logic. The sample run show below can be adapted to the new functions for matrices 
# and the behaviour will be the same.

makeCacheMatrix <- function(x = matrix()) {
#This function creates a special "matrix" object that can cache its inverse.
# m is defined locally and gloablly
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMyInverse <- function(MyInverse) m <<- MyInverse
        getMyInverse <- function() m
        list(set = set, get = get, setMyInverse = setMyInverse, getMyInverse = getMyInverse)
        # this list contains the functions to be invoked by cacheSolve.
                #$set
                #function (y) 
                #{
                #        x <<- y
                #        m <<- NULL
                #}
                #        
                #$get
                #function () 
                #        x
                #        
                #$setMyInverse
                #function (MyInverse) 
                #        m <<- MyInverse
                #
                #$getMyInverse
                #function () 
                #        m
}

cacheSolve <- function(x, ...) {
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
        
        
        m <- x$getMyInverse()
        # try to get inverted matrix from cache
        if(!is.null(m)) {
                # inverted data available in list
                message("getting cached data")
                return(m)
        }
        #possible improvement: need to validate here if matrix has changed??
        data <- x$get()
        m <- solve(data, ...)
        #function solve inverses the matrix m
        x$setMyInverse(m)
        # this sets the inverted matrix back into cache
        m
        # Return a matrix that is the inverse of 'x'
}