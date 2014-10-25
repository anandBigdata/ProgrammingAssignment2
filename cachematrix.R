## makeCacheMatrix function takes a matrix as input and creates a special matrix whose inverse can be cached. ## The input to makeCacheMatrix must be a square matrix.

## Let us say that we want to find the inverse of matrix matrix (c(1,2,3,4),2,2) and cache it. 
## Step 1 - get makeCacheMatrix by running: t <- makeCacheMatrix(c(1,2,3,4),2,2))
## Step 2 - get Inverse by running: cacheSolve(t)
## If you were to run cacheSolve(t) again, the inverse will be returned from the cache. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y)
           {
               x <<- y
               m <<- null
           }
    
    get <- function() x
    
    setInverse <- function(inverse) m <<- inverse
    
    getInverse <- function() m
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function takes the special matrix that you obtained from makeCacheMatrix. It returns
## inverse of the matrix that you provided as input to makeCacheMatrix and caches it when it runs first 
## time. Subsequent calls to cacheSovle for the special matrix will return the inverse from its cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
    
    	if(!is.null(m))
    	{
            message("getting data from cache")
            return(m)
        }
    
    	data <- x$get()
    	m <- solve(data)
    	x$setInverse(m)
    	m
}
