## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix : This function returns a list of functions set / get / setinverse / getinverse for a matrix
## passed as the parameter. 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
}



## cacheSolve : This function executes the functions created in makeCacheMatrix function. 

## if there is a matrix for m (that was already created during the initial run, it will be returned. 
## if there is no inverse calculation performed on m and is NULl, then it performs the following - 
##  a) an inverse is calculated is performed
## b) save the results into m 
## c) Returnm the m value 

cacheSolve <- function(x, ...) {
        m <- x$getinverse ()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse (m)
        m
}
