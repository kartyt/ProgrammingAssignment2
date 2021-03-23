## This is an assignment 2 for "R programming" course

## makeCacheMatrix function creates an R object that stores
## a matrix and its inversion. The function doesn't calculate the inversion itself.
## The assumption is that the matrix supplied is always a square. 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinvert <- function(solve){
                s <<- solve
        } 
        getinvert <- function() s
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}

## cacheSolve() function checks if the inversion of the matrix (set by
## makeCacheMatrix() function) is already calculated and store it in cache,
## if not, the function creates the inversion with solve() function

cacheSolve <- function(x, ...) {
        s <- x$getinvert()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinvert(s)
        s
}
