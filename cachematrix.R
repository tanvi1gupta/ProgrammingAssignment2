## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a numeric as an argument, 
##1. set the matrix using  $set
##2. get the matrix using $get
##3. get the inverse of the matrix using $getsolve
##4. set the inverse of the matrix using $setsolve

makeCacheMatrix <-function(x = numeric())
{
    inv <- NULL
    ##set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ##returns the matrix
    get <- function() x
    ##sets the inverse of the matrix
    setsolve <- function(solve) inv <<- solve
    ##returns the inverse of the matrix
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## takes argument as makeCacheMatrix and either fetches the inverse of the matrix if already computed else computes the 
## inverse of the matrix and stores it for future reference

cacheSolve<-function(x, ...)
{
 ## Return a matrix that is the inverse of 'x'
    inv <- x$getsolve()
    ##if inverse is present in the cache then return the inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ##else get the matrix 
    matrix <- x$get()
    ##compute inverse of the matrix using solve function
    inv <- solve(matrix, ...)
    #set the inverse of the given matrix
    x$setsolve(inv)
    inv
}
