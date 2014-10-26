## These functions allow the inverse of a matrix to be cached so we we not have 
## to calculate it each time we want the inverse: we can just look it up

## This function makes a "cache-able" matrix. It is really a list with several 
## values, one being the matrix we want to be able to cache the inverse of

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL                       ## creates the variable m within the function - m will be the inverse
        
        set <- function(y) {            ## this subfunction allows us to reset the matrix with the cached inverse
                x <<- y                 ## assigns value of y to x (x is the matrix)
                m <<- NULL              ## if set is called, a new matrix will be called in x, so we reset the inverse to be null again
        }                               ## we use the <<- operator so that x and m are avaliable outside of this subfunction, i.e. in the rest of the function
        
        get <- function(){              ## this subfunction returns the value of the original matrix
                x
        }
        
        setinverse <- function(solve){ 
                m <<- solve
        }
       
        getinverse <- function(){
                m
        } 
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will return the cached value of the inverse of the matrix specified
## If there is not a cached value it will calculate the inverse and store it in the cache

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
