## Put comments here that give an overall description of what your
## functions do
# "makeCacheMatrix" fucntion  creates a special matrix
# which can make and cache its inverse
# "cacheSolve" fucnition should compute the inverse of the matrix returned by makeCacheMatrix 
# if the inverse has already been calculated (and the matrix has not changed), 
# then the "cachesolve" should return the inverse from the cache.

## Write a short comment describing this function
# create a special matrix which can make and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<-inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Write a short comment describing this function
#"cacheSolve" fucnition computes the inverse of the matrix returned by makeCacheMatrix 
# if the inverse has already been calculated and the matrix has not changed, 
# then the "cachesolve" should return the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
