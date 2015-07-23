makeCacheMatrix <- function(x = matrix()) {
        y <- NULL
        getMx <- function() x
        ## getMx - return matrix that was passed to makeCacheMatrix
        getInvMx <- function() y 
        ## getInvMx - return inverse matrix from parent environment
        setInvMx <- function(InvMx) y <<- InvMx
        ## getInvMx - set inverse matrix in the parent environment
        list(getMx = getMx, getInvMx = getInvMx, setInvMx = setInvMx)
        ## result is the list of four functions
}


cacheSolve <- function(x, ...) {
        InvMx <- x$getInvMx()
        ## retrieve invesed matrix
        if(!is.null(InvMx)) {
                ## if it exists...
                message("getting cached data")
                ## then tell me about it...
                return(InvMx)
                ## and return cashed matrix
        }
        data <- x$getMx()
        ## If not, retrieve initial matrix...
        InvMx <- solve(data, ...)
        ## inverse it...
        x$setInvMx(InvMx)
        ## and add to cache.
        InvMx
        ##  Finally, return a matrix that is the inverse of 'x'.
}