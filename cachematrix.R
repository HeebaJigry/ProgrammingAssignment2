## R Language: Programming Assignment #2

## These two functions will take a matrix and calculate/return its inverse 
## ONLY IF THE INVERSE HAS NOT ALREADY BEEN CACHED
## if the matrix has been calculated, the cache is simply fetched and returned.


## The 1st function takes a matrix as an argument, the function has 4 methods.
## set() allows the variable x (the matrix) to be set and clears the cache (m)
## get() returns the matrix currently stored in x
## setCache() takes an argument and stores it in m (the cache)
## getCache() returns the current value of the cache (m)

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    
    set <- function(y) {
      
        x <<- y
        m <<- NULL
        }
    
    get <- function() x
    
    setCache <- function(cache) m <<- cache
    
    getCache <- function() m
    
    ##This next part will return a list of the methods 
    
    list(set = set, get = get,
         setCache = setCache,
         getCache = getCache) 

    }

## This function takes a matrix and possibly more arguments (set with "...")
## First the function sets m equal to the 'm' variable in makeCacheMatrix,
## if the new 'm' IS NOT empty, the function will return that value, done.
## Otherwise m is set to the inverse of 'data' (the original matrix).
## Then this new inverse value is set as the cache, then 'm' is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getCache()
    
    if(!is.null(m)) {
      
        message("getting cached data")
        return(m) 
        }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setCache(m)
    
    m
    }
