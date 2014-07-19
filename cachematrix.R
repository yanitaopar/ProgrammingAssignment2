## The two functions are used to create a special object that stores a matrix and cache's the reverse of the matrix.

## The function makeCacheMatrix creates a special matrix, which contains a function to set and get both the value of the matrix and the its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## The function cacheSolve calculates the inverse of the matrix created with the above function. 
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
