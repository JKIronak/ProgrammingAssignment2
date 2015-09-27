

## This function has 4 functions inside it
## "set" subtitutes x with y, and makes m NULL
## "get"just gets object stored in x
## "setinverse" stores the inverse in variable m
## "getinverse" returns the value of variable m


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
          x <<- y
          m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}



## cacheSolve will try to find inverse of a matrix, 
##firsts checks if an inverse was already made 
##in that case returns cached inverse and message
##if the inverse isn´t cached, makes an inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

