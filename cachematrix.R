## makeCacheMatrix to be called first followed by cacheSolve.
## Everytime a new matrix is used to calculate inverse, 
#### makeCacheMatrix has to be used first.


## function : makeCacheMatrix
## Input : matrix
## Output : List of functions, operation that are coupled to the data.

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    
    set <- function(y){
        x <<- y
        cache <<-NULL
    }
    
    get <- function() x
    
    ## Functions to share the cache 
    getCache <- function() cache
    
    setCache <- function(inverse) cache <<- inverse
    
    ##returns the list of functions created
    list(set=set,get=get,getCache=getCache,setCache=setCache)
    
}




## function : cacheSolve
## Input : object (from makeCacheMatrix)
## Output : matrix
## Description : This function checks if cache is null, if not the reads
#### from it else fills the cache for future references.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inverse <- x$getCache()
    if(!is.null(inverse)){
        print("Returning from cache")
    }else{
        data <- x$get()
        inverse <- solve(data)
        x$setCache(inverse)
    }
    
    return(inverse)
    
}


############### VERSION 2 #################################
## Simple Handshake between two function by sharing a cache as 
#### a global variable.
## Works only when there is only one matrix in consideration


# makeCacheMatrix <- function(x = matrix()) {
#     cache <<- NULL
#     x
# }
# 
# 
# cacheSolve <- function(x, ...) {
#     ## Return a matrix that is the inverse of 'x'
#     if(is.null(cache)){
#         cache <<- solve(x)
#     }else{
#         print("Returning from Cache")
#     }
#     
#     return(cache)
# }