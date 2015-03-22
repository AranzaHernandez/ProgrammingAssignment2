## The first part sets the matrix in the cache, and defines a list of functions to set 
## and get the original and inverse matrix.
##The next part cheks if the inverse was calculated and gives it back, otherwise
##calculates it.

## This defines a function that has a matrix for argument, and returns a list with 
##functions that will be used in the next part.

makeCacheMatrix <- function(x = matrix()) {
## Sets the inverse matrix as empty         
        inv <- NULL
## Defines the set function. It saves the matrix and the inverse in the cache       
        set <- function(y){
                x <<- y
                inv <<- NULL 
        }
## Defines the get function to call the original matrix
        get <- function() x
## Defines the setinv function to save the inverse matrix to the cache
        setinv <- function(inv) inv <<- solve
## Defines the getinv function to call the inverse matrix from the cache
        getinv <- function() inv
## Makes a list with this functions
        list(set = set, get = get, setinv = setinv, getinv = getinv )

}


## This function takes a list that should have the functions for get the original and
## inverse matix, and the function for set the inverse matrix. 
##If the inverse has been calculated, prints the result, otherwise it calculates it.

cacheSolve <- function(x, ...) {
## Gets the inverse matrix from the cache        
        inv <- x$getinv()
## If the inverse matrix has already been calculated, it just prints the result        
        if(!is.null(inv) ){
                inv
        }
## If the inverse han not been calculated, gets the original matrix and calls it "mat"
        mat <- x$get()
## and gets the invese matrix by the function solve       
        inv <- solve(mat,...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
