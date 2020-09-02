## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(matrix){
        x <<- matrix
        i <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setInv <- function(inverse){
        i <<- inverse
    }
    
    getInv <- function(){
        i
    }
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x <- x$getInv()
    
    if(!is.null(x)) {
        message("Getting Cached Data")
        return(x)
    }
    
    data <- x$get()
    
    m <- solve(data) %% data
    
    x$setInv(x)
    
    x
}
