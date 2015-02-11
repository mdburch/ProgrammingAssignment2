## These functions cache the inverse of a matrix
## by creating a special "vector" of functions 

## @brief  Takes a matrix as an input and
## 	     creates the getter and setter for the matrix
##         as well as the getter and setter for the inverse
## @param  x a matrix object
## @return a list of functions to get and set the matrix
##	     and get and set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) 
{
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## @brief  Takes a matrix and either retrieves the inverse
##	     of a matrix from cache or solves for the inverse
## @param  x a matrix object
## @return the inverse of the matrix 
cacheSolve <- function(x, ...) 
{
	  #Return the cached inverse of x if available
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
	  # Else solve for the inverse of x and return
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
