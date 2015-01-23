
##These two functions work together to save costly computing time 
##by storing a matrix and its inverse in memory. Essentially, the 
##inverse is solved for and stored in makeCacheMatrix so that when it is called later
##with cacheSolve(), it will be looked up rather than computed in real time



##creates a special "Matrix" object that is a nested function list that will:
##1. set the value of the matrix
##2. retrieve the value of the matrix
##3. set the value of the inverse of the matrix
##4. retrieve the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    ##retrieves stores the getInverse() function from makeCacheMatrix as 'm'
    m <- x$getInverse()
    ##check if inverse has been calculated, if so, it skips calculating
    ##and retrieves it. Also checks that matrix has not changed.
    if(!is.null(m) && m == x) {
        message("getting cached data")
        return(m)
    }
    ##retrieves the matrix from makeCacheMatrix and stores as 'data'
    data <- x$get()
    ##finds inverse of the matrix
    m <- solve(data, ...)
    ##sets the value of the inverse in the above function (makeCacheMatrix)
    x$setInverse(m)
    ##prints inverse
    m
}
