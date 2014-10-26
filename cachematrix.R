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
        
        setinverse <- function(solve){  ## this subfunction allows us to set the cached inverse m to the matrix 'solve' 
                m <<- solve
        }
       
        getinverse <- function(){       ## this subfunction returns the value of the cached inverse m. If it hasn't been set it will return NULL due to line 9
                m
        } 
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   ##This is the output and allows the functions to be called outside of the function makeCacheMatrix
}


## This function will return the cached value of the inverse of the matrix specified
## If there is not a cached value it will calculate the inverse and store it in the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                     ## This calls the getinverse function from x
        if(!is.null(m)) {                       ## Checks if there is already a cached value for the inverse
                message("getting cached data")
                return(m)                       ## If there is, it is returned (afer a message telling you it is the cached value) and the function ends
        }
        data <- x$get()                         ## calls get function from x. data variable is now the original matrix
        m <- solve(data, ...)                   ## Calculates the inverse and sets it as the variable m (which we would have previously set as NULL in line 38 if we are here)
        x$setinverse(m)                         ## uses the setinverse function to update the inverse in the x container environment
        m                                       ## prints the new inverse
}
