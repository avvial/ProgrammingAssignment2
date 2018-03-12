## makeCacheMatrix - function to save matrix and its inverse as a cached object for later retrieval

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                             # initialise object i
    set <- function(y) {                  # define a set function which
        x <<- y                           # assigns input to the parent environment object x and
        i <<- NULL                        # clear the value of i, so that it is reset whenever x is updated
    }
  
    get <- function() x                        # get function - retrieve the value of object x
    setinverse <- function(solve) i <<- solve  # set function - assigns the value of object i to the inverse of matrix x
    getinverse <- function() i                 # get function - retrieve the value of the inverse matrix object i
    list(set = set,                            # create a list object to return results from the function
         get = get,                
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve - function to extract the matrix and its inverse from objects create by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    i <- x$getinverse()                     # retrieve the inverse matrix from makeCacheMatrix
    if(!is.null(i)) {                       # if object i exists
        message("getting cached data")      # tell the user what is happening and
        return(i)                           # return the inverse matrix i to the user
    }
    data <- x$get()                         # if object i does not exist           
    i <- solve(data, ...)                   # solve for the inverse matrix
    x$setinverse(i)                         # save the inverse matrix i to the cache
    i                                       # return the inverse matrix i to the user
}