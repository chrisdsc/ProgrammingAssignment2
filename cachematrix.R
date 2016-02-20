## Calc the inverse of a matrix and cache the result to avoid 
## costly recomputation.

## Build a matrix object that can store its inverse
makeCacheMatrix <- function(x = matrix()) {
    xinverted <- NULL
    set <- function(mat) {
        x <<- mat
        xinverted <<- NULL
    }
    
    get <- function() x
    setinverted <- function (inverted) xinverted <<- inverted
    getinverted <- function() xinverted
    
    list(set = set, get = get, setinverted = setinverted,
         getinverted = getinverted)
}


## Compute the inverse of the x.get() matrix and cache the result.
## If the inverse was already computed: return the cached value
cacheSolve <- function(x, ...) {
    xinverted <- x$getinverted()
    if(!is.null(xinverted)){
        message('getting cached data')
        return(xinverted)
    }
    
    mat <- x$get()
    xinverted <- solve(mat, ...) ## caller can pass additional params to solve
    x$setinverted(xinverted)
    
    xinverted
}
