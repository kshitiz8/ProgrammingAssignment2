## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL;
        set <- function(y){
                x <<- y;
                i <<- NULL;
        }
        get <-  function() x
        setInverse <- function(inverse =  matrix()) i <<- inverse;
        getInverse <- function() i
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse);

}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve should retrieve the inverse 
##from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse();
        if(is.null(inverse)){
                inverse <- solve(x$get(),...);
                x$setInverse(inverse);
        }
        ## Return a matrix that is the inverse of 'x'
        inverse
}

