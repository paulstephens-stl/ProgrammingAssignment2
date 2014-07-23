## This pair of functions work together to create an object that acts as a 
## matrix that can cache the its inverse so it does not need to be computed 
## each time it is needed,
##
## The [makeCacheMatrix] function returns a list object that contains four 
## functions to allow the caller to set or get the base matrix or the cached 
## inverse of ## the matrix.
##
## The [cacheSolve] will compute and store the inverse of the matrix, but only 
## if the inverse has not already been computed in a previous call
##
## Example Usage: 
##
## rm(list=ls())
## i = makeCacheMatrix(matrix(c(1,3,6,9,0,2,5,7,9),3,3))
## m = cacheMatrix(i)
## m = cacheMatrix(i)
## round(i$get(),1)
## round(i$getInverse(),2)


## Returns a list with functions that can be used to get or set the matrix and 
## its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        #  a variable to store the cached inverse.  
        # Notice the "<<-" operator making this store in a higher context
        cachedInverse <<- NULL
        
        # a set of functions to get or set the matrix and its cached inverse. 
        set <- function(y) {x <<- y;cachedInverse <<- NULL}
        get <- function() {x}
        setInverse <- function(inverse) {cachedInverse <<- inverse}
        getInverse <- function() {cachedInverse}
        
        # return a list of functions to the caller
        list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
}



## returns the inverse of the matrix. The the inverse will only be computed 
## once. Repeated calls to this function will return the cached matrix.
cacheMatrix <- function(x, ...) {
        
        # get the inverse from the object and checked to see if it already has a value
        inv <- x$getInverse()
        if(!is.null(inv)) {
                # There is a value so no work to do.
                message("getting cached data")
        }
        else {
                # There is no value so we need to calculate and store the inverse.
                message("computing and storing inverse ")
                inv<- x$setInverse(solve(x$get()))
        }
        
        #return the inverse
        inv
}
