## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a list of 4 elements: 
##Using makeCacheMatrix$set(newmatrix) you`ll be able to change the matrix stored in makeCaheMatrix
##To display the matrix stored use the get() function
##setinv(inv) and getinv() are used to calculate the inverse of the matrix stored
##and to display it

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv<-function(inv) inv<<-inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve function firstly checks if the matrix stored has its inverse stored as well
##If not, then it calculates the inverse and stores it in the function makeCacheMatrix
##with the command line x$setinv(inv)

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached matrix")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data)
        x$setinv(inv)
        inv
}
