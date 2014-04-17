## makeCacheMatrix creates a cache for calculating the inverse of a matrix
## -- matrix must be square (pre-req for calculating the inverse)
## Functions:
## get() get the matrix store
## set(y) set a new matrix (clears the cached inverse)
## setinverse(y) sets the cached inverse
## getinverse() get the previous cached inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ##Set/Get for the matrix
    set <- function (y){
        x<<- y
        m <<-NULL
    }
    get <- function() x
    
    ##Set/Get for the Inverse Cache
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##cacheSolve takes in a cacheMatrix and calculates the inverse of it
##--- if the inverse has been cached in the cacheMatrix, 
##         this will be returned instead
##--- caches the inverse into the cacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data<- x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
