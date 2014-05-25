## There is two function there to operate on the matrix 
## First one is makeCacheMatrix which creats an "Object" like element like that one in
## the C++ or Java languages with methods fot getting and setting
## Second one cacheSolve which deals with this type of "Objects" and checks the
## inverse if it's cachec inside it then Gets it if it's not it evaluates it and
## Sets it.

## The function contains a list with the available operations on the matrix
## Which is 1- get it , 2- set it , 3- get the inverse , 4- set the inverse
makeCacheMatrix <- function(x = matrix()) {
        S <- NULL
        set <- function(y) {
                x <<- y
                S <<- NULL
        }
        get <- function() x
        setS <- function(solve) S <<- solve
        getS <- function() S
        list(set = set, get = get, setS = setS, getS = getS)
}

##This funcion simply checks if the inverse is already cached then displays it
## if it isn't then it evaluates it and then cahce it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        S <- x$getS()
        if(!is.null(S)) {
                message("getting cached inverse")
                return(S)
        }
        data <- x$get()
        S <- solve(data, ...)
        x$setS(S)
        S
}
