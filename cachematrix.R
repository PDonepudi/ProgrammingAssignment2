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

## cacheSolve function takes as the parameter the list that is created in "makeCacheMatrix" function. 
## The function tried to retrieve m (which should be the inverse of a matrix). 
## If m is a valid matrix, then it returns m and ends the function. 
## if there is no inverse calculation performed on m and is NULl, then it performs the following - 
##  a) read the matrix information using the get function. 
## b) calculate the inverse function using solve 
## b) save the results of m into the cachedmemory using setinverse function.  
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
