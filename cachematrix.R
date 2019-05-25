#The following function is to create a list within which functions are assigned and be retured to
#the parent environment so that $ operator can be used to access each function from the list.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The following function is to see if there is a cached value for inversed Matrix.
#If there is a valid cached value, this function can return it to the parent environment.
#This function is to calculate inverse of Matix and store it for the input argument
#if it is a of type makeCacheMatrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#testing the above functions:

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1

solve(n1)
myMatrixObject <- makeCacheMatrix(n1)
myMatrixInverse <- cacheSolve(myMatrixObject)
myMatrixInverse
