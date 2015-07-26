## use these two functions to reduce the redundant work of calculating the inverse of a matrix by caching the matrix,
## solve the inverse once and cache the inverse with the matrix

# this function stores the input matrix and the parameters of this matrix
makeCacheMatrix <- function(x = matrix()){
    inverse <- NULL
    set <- function(y){
        # set the input matrix into the matrix in use compulsively, and clear the inverse
        x <<- y
        inverse <<- NULL
    }
    get <- function() x #output the matrix in cache
    setInverse <- function(inv){
        #set the calculated inverse (by cacheSolve) into the cache
        inverse <- inv
    }
    #output the inverse of the matrix in store
    getInverse <- function() inverse 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# this funcion solve the inverse of the interested matrix if necessary
cacheSolve <- function(x, ...){
    #pass the parameter x, which is a 'makeCacheMatix' into the function
    #set the original value of inv
    inv <- x$getinverse
    if(!is.null(inv)){
        #there's already a value of inv in cache, work done
        message("getting cached data")
        return (inv)
    }
#else, need to calculate the inverse
    #get the matrix which need to be solved
    matrix <- x$get()
    #get the inverse
    inv <- solve(matrix, ...)
    #set it into the cached funcion
    x$setInverse(inv)
    #return the matrix that is the inverse of 'x'
    inv
}
