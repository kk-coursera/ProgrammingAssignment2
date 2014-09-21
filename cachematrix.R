## Matrix inversion is usually a costly computation and there may be some benefits to caching the inverse of a matrix rather than compute it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Retrieve cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        
}

#########################################################

##  Run makeCacheMatrix(x)
## > x <- rbind(c(1, 2, 3), c(2, 3, 1), c(3, 1, 2))
## > m <- makeCacheMatrix(x)
## > m$get()
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    2    3    1
## [3,]    3    1    2

## First run of CacheSolve(m)
## > cacheSolve(m)
##              [,1]        [,2]        [,3]
## [1,] -0.27777778  0.05555556  0.38888889
## [2,]  0.05555556  0.38888889 -0.27777778
## [3,]  0.38888889 -0.27777778  0.05555556

## Second run - retrieve from cache
## > cacheSolve(m)
## Retrieve cached data.
##              [,1]        [,2]        [,3]
## [1,] -0.27777778  0.05555556  0.38888889
## [2,]  0.05555556  0.38888889 -0.27777778
## [3,]  0.38888889 -0.27777778  0.05555556

#########################################################
