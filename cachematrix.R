## two functions are defined to create a special object which stores the matrix and caches its inverse

## makeCacheMatrix creates a list of 4 functions. Also it stores information locally. The matrix in 'x' 
## and the inverse in 'm'. When a new matrix is stored in by makeCacheMatrix, 'm' is set to NULL so that there 
## is no inverse matrix known.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                  ##stores the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                   ##variable$get() outputs stored matrix
        setinverse <- function(inverse) m <<- inverse        ##stores the inverse
        getinverse <- function() m           ##variable$getinverse() outputs stored inverse
        list(set = set, get = get,              ##output makeCacheMatrix 
             setinverse = setinverse,
             getinverse = getinverse)
}


##Checks if inverse is cached, if not calculates and inserts inverse into the cachematrix
cacheinverse <- function(x, ...) {
        
        m <- x$getinverse()             ## retreive inverse
        if(!is.null(m)) {               ## check if inverse exists, if yes return inverse
                message("getting cached data")
                return(m)
        }
        data <- x$get()                  ## if not, get matrix, calculate inverse and store and return inverse
        m <- solve(data, ...)
        x$setinverse(m)
        m
}