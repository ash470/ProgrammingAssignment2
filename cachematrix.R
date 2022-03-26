## Put comments here that give an overall description of what your
## functions do

##There are 2 functions:makeCacheMatrix consists of set,get,setinv,getinv
##library(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  #initializing inv as null
        set <- function(y) {
                x<<-y
                inv<<- NULL
                }
        get <- function()x                      #function to get matrix x
        setinv <- function(inverse)inv<<-inverse
        getinv <- function(){
                inver <- ginv(x)
                inver%*%x 
                }       #function to obtain inverse of matrix
 list(set = set, get = get,
      setinv = setinv,
      getinv = getinv)
}


## Write a short comment describing this function
## This is used to get cache data

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {             #checking whether inverse is null
                message("getting cached data!")
                return(inv)             # returns inverse value
                }
        data <- x$get()
        inv <- solve(data,...)          # calculates inverse value
        x$setinv(inv)
        inv                             ##returns inverse of x matrix
}

f<-makeCacheMatrix(matrix(1:8,2,4))
f$get()
f$getinv()
cacheSolve(f)
