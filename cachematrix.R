## The following two functions uses R scoping rules to cache inverse of a matrix so if the content does
## not change then the result of inverse matrix is retured from cache rather than computing the value again
## in time consumping computations. <<- operator in R is used to assign a value to an object 
## in an environment that is different from the current environment.


## This fucntion creates a list containing functions to
##      set the value of the matrix
##      Get the value of the matrix
##      set the value of the inverse of the matrix
##      get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## setter funtion
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get function to return the matrix
        get <- function() x
        ## set the inverse to varialbe m
        setinverse <- function(inv) m <<- inv
        ## ger the inverse from varialbe m
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x' either from already computed value from Cache or
## Calculates the inverse and return the result.

cacheSolve <- function(x, ...) {
        
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        ##calucutes the inverse of the matrix by using R built in Funtion Solve if result is not in Cache
        m <- solve(data)
        x$setinverse(m)
        return(m)
}

