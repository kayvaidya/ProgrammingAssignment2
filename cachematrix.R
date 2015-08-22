## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(invenrtibleMatrix = matrix()) {
        ## @invenrtibleMatrix: a square invertible matrix
        
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                invenrtibleMatrix <<- y
                inv <<- NULL
        }
        get = function() invenrtibleMatrix
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(cachedMatrix, ...) {
        ## @cachedMatrix: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = cachedMatrix$getinv()
        
        
        if (!is.null(inv)){ ## check if the matrix is already cached
                message("getting cached data")
        } else {
        	# otherwise, calculates the inverse 
	        mat.data = cachedMatrix$get()
	        inv = solve(mat.data, ...)
	        
	        cachedMatrix$setinv(inv) ##set the value of the cached matrix

        }
        return(inv)
        
        
}