## Functions create a matrix and store it in dMatrix
## Create a cached version
## Sets dMatrix into the cached version

### Run sequence ###
        #   dMatrix <- matrix(1:4,2,2)
        #   a<-makeCacheMatrix(dMatrix)
        #   cacheSolve(a)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Uses cached version if this has already been calculated
        #   Run the cacheSolve(a) statement a second time 
        #   will display "getting cached data" because it didn't recalculate

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}

### Tested using the following matrix configurations ###

## Successful ##
#   dMatrix <- matrix(1:4,2,2)
#   dMatrix <- matrix(trunc(rnorm(64*64)*100), 64,64)
#   dMatrix <- matrix(trunc(rnorm(512*512)*100), 512,512)

## Fails ##
        #   dMatrix <- matrix(1:9,3,3) --- FAILS on this, why?
        #   dMatrix <- matrix(trunc(rnorm(1:100)),10,10) --- FAILS on this, why?
