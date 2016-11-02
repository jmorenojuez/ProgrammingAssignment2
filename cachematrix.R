## Programming assignment 2 for Coursera's Data Science Course
## Author: José María Moreno Juez
## Date: 2016/11/03
## Usage:
## Create an invertive matrix mat
## > mat <- matrix(c(1,3,2,4),2)
## > mat
## [,1] [,2]
##[1,]    1    2
##[2,]    3    4
##> 
## Source the R file containing the functions    
##    > source('cachematrix.R')
##Call makeCacheMAtrix over mat variable
##> cmat <- makeCacheMatrix(mat)
## First call to cacheSolve creates the inverse matrix
##> cacheSolve(cmat)
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
## The second and sucesives calls to cacheSolve function uses the cached value  
##> cacheSolve(cmat)
##getting inverse matrix from cache
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
##> 

## Function that creates a special "matrix" object that could cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function (y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setInvMat <- function (invMat) i <<- invMat
    getInvMat <- function() i
    list(set = set, get = get,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
    
}


## Function that uses the special matrix returned by makeChacheMatrix to get the inverse of a matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInvMat()
    if (!is.null(i)) {
        message("getting inverse matrix from cache")
        return (i)
    }
    # Inverse matrix hasn't been calculated previously 
    data <- x$get()
    ## Matrix must be inversible!
    i <- solve(data)
    x$setInvMat(i)
    i
}
