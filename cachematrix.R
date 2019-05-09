makeCacheMatrix <- function(x = matrix()) {

    ## Create functions to set, get, setinverse, and getinverse.  These are
    ## required for cacheSolve function to run properly.
    
    ## Create empty matrix, invmatrix
    invmatrix <- NULL
    
    ## Set function
    set <- function(y) {
        x <<- y
        invmatrix <<- NULL
    }
    
    ## Get function
    get <- function() x
    
    ## Setinvserse funtion
    setinverse <- function(inverse) invmatrix <<- inverse
    
    ## Getinverse function
    getinverse <- function() invmatrix
    
    ## Returns the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


cacheSolve <- function(x, ...) {
    
    ## This function returns inverse of matrix x.  It assumes that x is a 
    ## solvable matrix.  If inverse has already been solved, then this
    ## function will return a cached version of the solution.  Intent is to
    ## speed up the return of the solution.
    
    ## Getinverse stores matrix into invmatrix
    invmatrix <- x$getinverse()
    
    ## If matrix has been solved alredy, invmatrix is returned from cache
    if(!is.null(invmatrix)) {
        message('getting cached data')
        return(invmatrix)
    }
    
    ## If matrix has not been solved, then gets matrix and solves
    data <- x$get()
    invmatrix <- solve(data, ...)
    
    ## Setsinverse 
    x$setinverse(invmatrix)
    
    ## Returns invmatrix
    invmatrix
        
}
