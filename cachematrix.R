## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#the makeCacheMatrix is setting up the environment where values of x and i (the inverse matrix) could be retrived by the cacheSolve function defined later. The makeCacheMatrix turns a list of four function, set(), get(), setinverse() and getinverse(). 
#set() can be called when the input matrix is changed to a new one and it initialises the values of i. 
#get() retrives the value of x and will be passed to the solve function in the cacheSolve function as input data to calculate the inverse.
#setinverse() saves the calculated inverse matrix (called in the cacheSolve function)
#getinverse() retrives the saved inverse matrix and gets returned when the same calculation is required the second time onwards

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #initialise the object i so that it exists
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
#the cacheSolve function works in conjunction with the makeCacheMatrix function and calles the functions defined in makeCacheMatrix function to either calculate the inverse when a new matrix is put in or returns the existing inverse when the same matrix calculation is required again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


