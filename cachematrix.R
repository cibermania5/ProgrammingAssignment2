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
