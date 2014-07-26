## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a a list containing a function to set the matrix, get the matrix, set the inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i<-null
    ## Set the matrix
    set<-function(matrix){
        m<<-matrix
        i<<-null
    }
    ## Get the matrix
    get<-function(){
        ##return the matrix
        m
    }
        
    
    ## Set the inverse of the matrix
    setInverse<-function(inverse){
        i<<-inverse
    }
    ## Get the inverse of the matrix
    getInverse<-function(){
        ## Return the inverse
        i
    }
    ## Return a list
    list(set=set, get=get,
        setInverse=setInverse,
        getInverse=getInverse)

}



## Write a short comment describing this function
## The function calculates the inverse of the matrix created with the above function. It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        ## Return the inverse if it is set
        if(!is.null(m)){
            message("getting cashed data")
            return(m)
        }
        ## Get the matrix from the object
        data<-x$get()
        ## Get the inverse through multiplication
        m <- solve(data) %*% data
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m
}
