
##I set the input x as a matrix
##I assumed that matrix applied here is invertable so I assigned NULL to a variable inv
##Then i set the value of matrix using another function
##So outside the set function i will get the value of the matrix
##Then i set the value of inverse using setinverse
##Then i wrote the function for getting he value of inverse and assigned it o variable getinv
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                          x <<- y
                          inv <<- NULL
                           }
        get <- function()x
        setinv <- function(inverse)inv <<- inverse
        getinv <- function(){
                            inver <- ginv(x)
                            inver%*%x
                            }
        ##Then i created the list here
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


       

cacheSolve <- function(x, ...) {
        ##Below line returns the matrix thatt is inverse of x and assign it to inv
         ##For checking if the inverse is already computed,so if the inverse is retreived from cache
        ##then a message "gettting cached data" would be displayed and the inverse would be returned
        ##otherwise wse need to compute the matrix so for computing matrix solve function is used
        ##Then i sett the value of inverse in the cache using setinverse function
        inv <- x$getinv() 
        if(!is.null(inv)){
                           message("getting cached data!")
                           return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
}
