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
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
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
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setsolve(inv)
    inv
}
