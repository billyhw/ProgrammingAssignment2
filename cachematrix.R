## Inverting large matrices can be a computationally costly
## operation. One way to avoid such computation is to cache
## the inverse of a matrix. When matrix inversion is performed
## again later, first check whether a cached inverse has been computed. 
## If so, one can simply retrieve the inverted matrix instead 
## of re-evaluating the inverse. This program makes use of the concerpt of
## lexical scoping to achieve this goal.

## Many comments and descriptions below are adapted from the tutorial by
## Len Greski
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/
##    rprog-breakingDownMakeVector.md

## makeCacheMatrix provides getter and setter functions
## for a matrix and its inverse. The environment of this
## function contains the cached matrix and its inverse.

## Input: 
## x: a matrix to be inverted
## Output: an object with the getter and setter functions 
## for the cached matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

		# initialize the inverse to NULL

        x_inv <- NULL 
        
        # set() is the setter function for a matrix

        set <- function(y) {

        		# set the input matrix in the makeCacheMatrix
                # environment to y. Note the "<<-" operator
                # set x in the parent frame of set(), i.e.
                # the makeCacheMatrix environment
                
                x <<- y 
                
                # assign the x_inv in the makeCacheMatrix 
                # environment to NULL

                x_inv <<- NULL 

        }
        
        # get() is the getter function for the
        # matrix x in the makeCacheMatrix environment

        get <- function() x 

        # setinverse() is the setter function for the inverted matrix.
        # It assigns x_inv in the makeCacheMatrix environment.

        setinverse <- function(inverted_matrix) x_inv <<- inverted_matrix

        # getinverse() is the getter function for the inverted matrix
        # It returns the inverted matrix residing in the makeCacheMatrix 
        # environment.

        getinverse <- function() x_inv

        # return the setters and getters defined above

        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve is the function used to determine
## whether a cached matrix and its inverse
## exist, and accordingly return the cached inverted
## matrix if exist, or otherwise call the solve() function
## to compute, cache, and return the inverted matrix

## Input: 
## x: a makeCacheMatrix object
## Output: the inverted matrix

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        # first check whether an inverted matrix exists
        # in the environment of the input makeCacheMatrix object

        x_inv <- x$getinverse() 

        # if an inverse exist, simple return it

        if(!is.null(x_inv)) { 
                message("getting cached data")
                return(x_inv)
        }

        # otherwise, get the matrix passed to the
        # input makeCacheMatrix object x.

        data <- x$get() 

        # and then compute its inverse,

        x_inv <- solve(data, ...)

        # and cache the inverse matrix

        x$setinverse(x_inv)

        # return the inverse matrix

        x_inv

}

## Some sample code below to test the functions
## Not Run

set.seed(100)
tmp <- crossprod(matrix(rnorm(300000), 1000, 300)) # ensure matrix invertible
x <- makeCacheMatrix(tmp)
system.time(output1 <- cacheSolve(x))
system.time(output2 <- cacheSolve(x)) # see if cached matrix is returned
all(output1 == output2) # the two inverted matrix should be equal

tmp2 <- crossprod(matrix(rnorm(300000), 1000, 300))

x$set(tmp2)
system.time(output2 <- cacheSolve(x)) # matrix changed, recompute inverse
system.time(output2 <- cacheSolve(x)) # cached matrix return

tmp3 <- crossprod(matrix(rnorm(300000), 1000, 300))
y <- makeCacheMatrix(tmp3)
system.time(output3 <- cacheSolve(y)) # matrix changed, recompute inverse
all(output1 == output3) # should be a different matrix