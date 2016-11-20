## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The purpose of this function is to cache the inverse of the values in matrix and store them.
## The set and get are used to assign values and retrieve
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     setinversecol <- function(inverse) inv <<- inverse
     getinversecol <- function() inv
     list(set=set, get=get, setinverse=setinversecol, getinverse=getinversecol)
 }


## Write a short comment describing this function
## This is the second function that will return cached value of matrix. The first run of any new matrix wouldnt be cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'    
     inv <- x$getinverse()
     if(!is.null(inv)) {
         message("This data will be from the cache.")
         return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
}

##> x = rbind(c(1, 2), c(1, 8))
##> m = makeCacheMatrix(x)
##> m$get()
##     [,1] [,2]
##     [1,]    1    2
## [2,]    1    8
##> cacheSolve(m)
##           [,1]       [,2]
## [1,]  1.3333333 -0.3333333
## [2,] -0.1666667  0.1666667
## > cacheSolve(m)
## This data will be from the cache.
##           [,1]       [,2]
## [1,]  1.3333333 -0.3333333
## [2,] -0.1666667  0.1666667
