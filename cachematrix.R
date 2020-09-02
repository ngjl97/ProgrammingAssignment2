## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Initiate the inverse property
    i <- NULL
    
    ## Set the matrix
    set <- function(matrix){
        x <<- matrix
        i <<- NULL
    }
    
    ## Get the matrix
    get <- function(){
        x
    }
    
    ## Setting inversion of matrix
    setInv <- function(inverse){
        i <<- inverse
    }
    
    ## Getting inversion of matrix
    getInv <- function(){
        i
    }
    
    ## Return a list of methods
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x <- x$getInv()
    
    ## Return the inverse if the matrix is already set
    if(!is.null(x)) {
        message("Getting Cached Data")
        return(x)
    }
    
    ## Get the matrix from object
    data <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %% data
    
    ## Set the calculated inverse to the object
    x$setInv(x)
    
    ## Return the matrix
    x
}
