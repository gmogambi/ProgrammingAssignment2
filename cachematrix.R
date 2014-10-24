## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## It stores a martix and a cached value of the inverse of the matrix.
## 1. setMatrix: set the value of a matrix
## 2. getMatrix: get the value of a matrix
## 3. cacheInverse: get the cached value (inverse of the matrix)
## 4. getInverse: get the cached value (inverse of the matrix)
## 5. cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## 6. If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache

## Create the makeCacheMatrix function:

makeCacheMatrix <- function(m = numeric()) ## holds the cached value
{
    mc <- NULL ## cache set to NULL because for the first time there is nothing to cache
    
    ## set the value of a matrix
    setMatrix <- function(sMatrix)  ## This matrix is stored here
    {
        m <<- sMatrix
        
        mc <<- NULL ## since the matrix is assigned a new value, flush the cache
    }
    
    ## get the value of a matrix
    getMatrix <- function() ## This will return the stored matrix
    {
        m
    }
    
    ## get the cached value (inverse of the matrix)
    cacheInverse <- function(cInverse) ## This is the cache of the Inverse 
    {
        mc <<- cInverse
    }
    
    ## get the inverse cached value (inverse of the matrix)
    getInverse <- function()## get the inverse cached value 
    {
        mc
    }
    
    ## a list of all functions is returned
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

## The cacheSolve function below calculates the inverse of a special "matrix" created with
## makeCacheMatrix
cacheSolve <- function(c, ...) 
{
    Matrixinverse <- c$getInverse()## get the cached value
    
    if(!is.null(Matrixinverse)) ## if there is a cached value then return it
    {
        message("getting cached data")
        return(Matrixinverse)
    }
    ## if catched value is not found, get the matrix, calculate the inverse and store it in the cache
    data <- c$getMatrix()
    Matrixinverse <- cInverse(data)
    c$cacheInverse(Matrixinverse)
    
    ## return the inverse of the matrix
    Matrixinverse
}

##** This program was written and compiled following the examples given in 
##** Programming Assignment 2: Lexical Scoping