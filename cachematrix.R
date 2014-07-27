## cachematrix.R
## Purpose: 
##               To cache -store- the value of the inverse of a given matrix.
## How it works:
##               we will use two functions, the first one to cache the value of the inverse; the second one to actual calculate the inverse values, if already calculated for a given matrix, it will simply return the stored value.       
## I will use the model -example- provided as it will need just a very few changes.



## The following function will set and get the values for a given matrix (the object of the calculation)

makeCacheMatrix <- function(x = matrix()) {

            m <- NULL  ## initializing the value
        
            ## setting the matrix value...
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            ## defining the get and set for the values
        
            get <- function() x
            setinv <- function(inverse) m <<- inverse
            getinv <- function() m
            list(set = set, get = get,
                 setinv = setinv,
                 getinv= getinv)

}


## Calculate and retrieve the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinv() ## return the inverse of the matrix
            
             ## checking if the value is already cached if it is, the print the message and return the cached value.
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()  ## get the value to solve
            m <- solve(data, ...)   ## solving the matrix, note that here we could easily check if the matrix is a square one.
            x$setinv(m)   ## caching...
            m    ## return the value...
}

## test case
##> rm(x)
##> X=matrix(3:6,2,2)
##> x<-makeCacheMatrix(X)
##> x$get()
 ##    [,1] [,2]
##[1,]    3    5
##[2,]    4    6
##> cacheSolve(x)
##     [,1] [,2]
##[1,]   -3  2.5
##[2,]    2 -1.5
##> cacheSolve(x)
##getting cached data
##     [,1] [,2]
##[1,]   -3  2.5
##[2,]    2 -1.5
##

##> 

##Testing with Octave:

## octave:1> A=[
## > 3 5
## > 4 6
## > ]
## A =

##    3   5
##    4   6

## octave:2> pinv(A)
## ans =

##   -3.0000   2.5000
##    2.0000  -1.5000

##   octave:3> A*pinv(A)
##   ans =

##      1.0000e+00   1.7764e-15
##     -3.5527e-15   1.0000e+00


