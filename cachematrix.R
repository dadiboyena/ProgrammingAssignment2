## Matrix inversion has the advantage to cache the inverse of a matrix in order to avoid 
## the repeated computation. The following functions computes to cache the inverse of a matrix.

    ## makeCacheMatrix creates a list containing a function to
            ## a. set the matrix value
            ## b. get the matrix value
            ## c. set the value of "inverse" of the matrix
            ## d. get the value of "inverse" of the matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <-function(y){
        x <<- y
        m <<- NULL
    }
    
    get <-function() x
    setmatrix <-function(solve) m<<- solve
    getmatrix <-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## The following function returns the inverse of the matrix. Firstly, It checks if
## the inverse has already been calculated. If YES, it gets the result "GETTING CACHED DATA" 
## and skips the calculation. If NOT, it calculated the inverse, sets the value in the cache via
## setmatrix function.

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
        message("GETTING CACHED DATA")
        return(m)
    }
    matrix <-x$get ()
    m <-solve(matrix, ...)
    x$setmatrix(m)
    m
}

## An Example of the run is provided below for convenience
        ## x = rbind(c(1, -1/3), c(-1/3, 1))
        ## m = makeCacheMatrix(x)
            ## m$get()
            ## [,1]       [,2]
            ## [1,]  1.0000000 -0.3333333
            ## [2,] -0.3333333  1.0000000

        
        ## "first run"
            ## cacheSolve(m)
            ## [,1]  [,2]
            ## [1,] 1.125 0.375
            ## [2,] 0.375 1.125

        ## retrieving the cache (second run)
            ## GETTING CACHED DATA
            ## [,1]  [,2]
            ## [1,] 1.125 0.375
            ## [2,] 0.375 1.125