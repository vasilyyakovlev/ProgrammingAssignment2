## "makeCacheMatrix": This function creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
## Setting the function.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) i <<- solve
        getSolve <- function() i
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## "cacheSolve": This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

## Returning a matrix that is the inverse of 'x'.
        i <- x$getSolve()
        if(!is.null(i)) {

## If inverse is cached before, just returns its value.
                message("getting cached data")
                return(i)
        }
## Geting the matrix from the object.
data <- x$get()

## Calculating the inverse.
i <- solve(data, ...)

## Seting the inverse to the object.
x$setSolve(i)

## Returning the matrix.
i
}
