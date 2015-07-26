##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly.

##Below was a pair of function that cache the inverse of a matrix.
##For the functions below:
##~Compute the inverse of a square matrix can be done with the solve function in R.
##~Assume that the matrix supplied is always invertible.

##This function is makeCacheMatrix, it is used to create a "special" matriX object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y)
        {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() return(x)
        setinv <- function (inv) inverse <<- inv
        getinv <- function () return(inverse)
        return (list(set= set, get = get, setinv = setinv, getinv = getinv))
}


##This function is cacheSolve and it is used to compute the inverse of the 
##special "Matrix" and returned by "makeCacheMatrix" above.
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if(!is.null(inverse)) 
        {
                message("Getting Cached Data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        return (inverse)
       
}
