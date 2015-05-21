### Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## Below are two functions that are used to create a special "matrix" object that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL 				  ## 1
        }
        get <- function() x  				  ## 2
        setinverse <- function(inverse) invM <<- inverse  ## 3
        getinverse <- function() invM			  ## 4
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The second function returns the inverse of the matrix. It inlcudes the following:  
## 1. It checks to see if the inverse has already been computed. 
## 2. If so, it gets the result from the cache and skips the computation. 
## 3. Return a matrix that is the inverse of 'x'
## 4. Otherwise, it computes the inverse and sets the value in the cache via setinverse function.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x,...) {
        invM <- x$getinverse() 				  ## 1
        if(!is.null(invM)) {    			  ## 2
                message("getting cached data")
                return(invM) 				  ## 3
        }
        data <- x$get()
        invM <- solve(data,...)
        x$setinverse(invM)
        invM					       	  ## 4
}