## Put comments here that give an overall description of what your
## functions do

## Together makeCacheMatrix and cacheSolve take in a matrix, create and inverse matrix
## solve the inverse matrix and cache the solution for future use.


## makeCacheMatrix takes in a matrix and return 4 functions: set, get, setinv and getinv
## set: sets the value of the matrix
## get: returns the value of the matrix
## setinv: sets the value of the inverse matrix
## getinv: gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # returns x as a matrix
    get <- function() x
    
    # calculates the inverse of x and puts it in the cache
    setInverse <- function(inverse) m <<- inverse
    
    # takes the inverse value 
    getInverse <- function() m
    
    #returns all the functions
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## this function looks to see if function is in the cache and returns it
## otherwise calculates it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    # if the inverse is present in the cache, return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    
    #returns it!
    m
}
