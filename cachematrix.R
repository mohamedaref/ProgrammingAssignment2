## This two function are inverting a square matrix and use global variables techniques 
## to catch if this matrix is already submitted before or not.

## makeCacheMatrix function desgined to match the OOP concepts
## it is containg some of set and get functions(in OOP its called methods), 
## no logic in this function at all.
## this will help you to extend your code in futuer through adding new functions inside it

makeCacheMatrix <- function(x = matrix()) {
        
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setmatrixInv <- function(minv) a <<- minv
        getmatrixInv <- function() a
        list(set = set, get = get,
             setmatrixInv = setmatrixInv,
             getmatrixInv = getmatrixInv)

}


## this function is designed to use makeCacheMatrix function and its methods.
## it will return the inverse matrix by using solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getmatrixInv()
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setmatrixInv(a)
        a
}
