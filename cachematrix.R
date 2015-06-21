## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL # to be filled and returned
        
        set <- function(inversa) {
                x <<- inversa # changes the x value to inversa
                m <<- NULL
        }
        
        get <- function() x
        
        setmm <- function(inversa)
        {
                m <<- inversa # stores inversa into m
        }
        
        getmm <- function() m
        
        list(set = set, get = get,
             setmm = setmm,
             getmm = getmm, asignacion=asignacion)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getmm()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmm(m)
        m
}