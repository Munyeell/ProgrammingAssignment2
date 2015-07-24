## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Below function creates a "special" matric object 
##that can cache its inverse.
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


## Write a short comment describing this function

##This function will compute the inverse of the special "Matrix"
##and returned by "makeCacheMatrix" above.
##If the inverse has already been calculated
##(and the matrix has not changed)
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
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
        ## Return a matrix that is the inverse of 'x'
}
