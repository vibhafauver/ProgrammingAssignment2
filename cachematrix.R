makeVector <- function(x = numeric()){
    m <- NULL
    ##defines a function to set the vector, x, to a new vector, y, and resets the mean, m, to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x #returns the vector, x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    #returns the 'special vector' containing all of the functions just defined
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

makeCacheMatrix <- function(x = matrix()){
    ## This function creates a special "matrix" object that can cache its inverse.
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) s <<- inverse
    getinverse <- function() s
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cachesolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    mat <- x$get()
    s <- solve(mat, ...)
    x$setinverse(s)
    s
}