## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix() creates an R object that stores a matrix and its inverse
# cacheSolve requires an argument that is returned by makeCacheMatrix() in order to get back the inverse of the cached matrix that is stored in makeCacheMatrix()

## Write a short comment describing this function
# This function builds a set of functions and returns them inside a list aiming those functions
# will feed a second function called cacheSolve


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
# cacheSolve() will either compute the inverse of the matrix stored in makeCacheMatrix() or send it back if cachesolve() had already been ran

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    m <- solve(data)
    x$setsolve(m)
    m
    
}



## Try it with a simple example

my_mat <- matrix(seq(1:4), nrow = 2, ncol = 2, byrow = TRUE)
# View my_mat

my_mat_intermediate <- makeCacheMatrix(my_mat)

cacheSolve(my_mat_intermediate)

