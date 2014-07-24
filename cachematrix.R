## Inverse of a matrix 

##### Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function (X = matrix ()) {
        M <- NULL
        set <- function (Y)  {
                X <<- Y
                M <<- NULL
                
        }
        get <- function() X
        setInverse <- function(solve)  M <<- solve
        getInverse <- function () M
        list (set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}
        



#####  Computes the inverse of the special "matrix" or retrieve it from the cache if it already exists.

cacheSolve <- function(x, ...) {
        M <- x$getInverse()
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- x$get()
        M <- solve(data, ...)
        x$setInverse(M)
        M
}

