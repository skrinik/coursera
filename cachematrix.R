## The makeCacheMatrix function takes in some matrix (can be defaulted to an empty matrix which will store NA as the inverse), and returns a list of functions that can store, set or retrieve information about the matrix you passed in/created. This list object will later be used as the parameter for cacheSolve where the function checks to see if the solve function has been called yet or not. This is determined by the value of m which is defaulted to NULL if solve hasn't been called yet. But, if solve has been called, the value of m now stores the cached inverse of the original matrix.
# -------------------------------------------------------------------------------------------------------------#

## makeCacheMatrix as stated above, takes in some matrix (can be defaulted to an empty matrix which will store NA as the inverse), and returns a list of functions that can store, set or retrieve information about the matrix you passed in/created.

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

#CALL FOR LIST
list_fun <- makeCacheMatrix() #make sure to input non-default value if needed.

# -------------------------------------------------------------------------------------------------------------#
## cacheSolve as stated previously, checks whether the solve function has been called within the Global Env. and has altered the 'm' variable within the function. If so, it reuses 'm'. If not, it calculates the inverse matrix and then caches the value for later. cacheSolve takes a list as an parameter (obviously output from makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

# CALL FOR RESULT
cacheSolve(list_fun)
