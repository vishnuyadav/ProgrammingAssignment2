#The  scrpit contains two function makeCachematrix() and cacheSolve() to find the inverse of a given matrix
# The Matrix supplied as argument is cached, so that recalculation is not needed


#Gives a bunch of function on a given matrix to calculate the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        get <- function() x_inverse
        setinverse <- function(solve) x_inverse <<- solve
        getinverse <- function() x_inverse
        list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}

#Calculates the inverse of a matrix, if it is already not calculated before. i.e. Cached before
# if inverse is cached there are no calculation done to recalculate it again
cacheSolve <- function(x, ...) {
        x_inverse <- x$getinverse()
        if(!is.null(x_inverse)) {
                message("getting cached data")
                #function exits here if inverse is already cached
                return(x_inverse)
        }
        #calculate the new inverse
        data <- x$get()
        x_inverse <- solve(data, ...)
        x$setinverse(x_inverse)
        
        return(x_inverse)
}