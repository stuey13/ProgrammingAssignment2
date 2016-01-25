## With these functions, for invertible matrix it will be enough to compute
## its inverse only once! Then the inverse will be cached.

## "makeCacheMatrix" function creates a useful matrix object (it becomes
## a list of functions).

makeCacheMatrix <- function(A = matrix()) {
        inv <- NULL
        set <- function(B) {
                A <<- B
                inv <<- NULL
        }
        get <- function() A
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## "cacheSolve" function checks for a given matrix if its inverse is already
## computed. If it's not, it computes the inverse and then it is cached for
## future use. It returns the inverse.

cacheSolve <- function(A, ...) {
        inv <- A$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        C <- A$get()
        inv <- solve(C, ...)
        A$setinverse(inv)
        inv
}
