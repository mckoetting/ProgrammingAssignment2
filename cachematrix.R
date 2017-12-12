## The functions below may be used to collectively: 
## -create or modify a list object containing a matrix and its inverse, and
## -compute and store the matrix's inverse.


## "makeCacheMatrix" creates and returns a list containing the input matrix, its
## inverse (if it has been computed by "cacheSolve"), and functions to retrieve
## ("get()") or modify ("set()") the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m<<-inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
        

}


## "cacheSolve" is used to compute and store the inverse of the matrix within a
## stored CacheMatrix list (see "makeCacheMatrix" above).  

## If the inverse has has been previously computed and stored, "cacheSolve" 
## returns the stored inverse matrix.  If the inverse has not been previously 
## computed/stored or if the matrix has been modified, "cacheSolve" will compute
## and return the matrix inverse and store it within the input CacheMatrix list.
        ##Precondition:  The input matrix must be an invertible, square matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
