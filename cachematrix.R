## This function cache the matrix inverse so we can reduce the computation costly 
## of repeatedly comput it.

## makeCacheMatris is a function that creates a special "matrix" object that can cache its inverse.
##

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed)
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getmm()
        if(!is.null(m)) { ##Chek if the inverse matrix is cache
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) ##Compute the inversa matrix if is necessary
        x$setmm(m) ## Store the value
        m
}