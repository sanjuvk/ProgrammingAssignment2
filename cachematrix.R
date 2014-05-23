## This program calculate the inverse of a square matrix and 
## caches the result. If the inverese of the same matrix is 
## requested again then the cached value is returned instead 
## of performing the time consuming calculation again. 

## makeCacheMatrix function implements methods (sub functions) to set (store) and  
## get (retrieve) a matrix and the calculated invese of that matrix. 

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## cacheSolve function checks whether the inverse of a matrix already exist in the cache.
## if it exists then returns the cached value else calculates the inverse and return 
## calculated value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data...")
                    return(m)
            }
            else{
                    message("calculating the inverse...")
                    data <- x$get()
                    m <- solve(data, ...)
                    x$setinverse(m)
                    m
            }
         m 
}       
