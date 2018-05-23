## Goal: Writing a pair of functions to cache the inverse of a matrix rather than computing it repeatedly 
##       by taking advantage of the scoping rules of the R language
##
## last modified by Yul Young Park on 05/23/2018

## 
## This function creates a special "matrix" object that can cache its inverse.
## it is a list containing following functions:
## set() sets the value of the matrix
## get() gets the value of the matrix
## set_inv() sets the value of the inverse matrix
## get_inv() gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y){
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    set_inv <- function(calc_inv) m_inv <<- calc_inv
    get_inv <- function() m_inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
##  then cacheSolve should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix and set the value of the inverse matrix
##  in the cache via the set_inv() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m_inv <- x$get_inv()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    data_mat <- x$get()
    m_inv <- solve(data_mat, ...)
    x$set_inv(m_inv)
    m_inv
}
