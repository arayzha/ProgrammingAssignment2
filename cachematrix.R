## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #make initial value for inverse (i)
	i <- NULL
	
	#set the value of x
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
	
	#set the inverse value
        setinv <- function(inverse) i <<- inverse
        
	#get the inverse value
	getinv <- function() i
	
	#listing the result for return
        list(set = set, get = get,
             setinv = setinv ,
             getinv = getinv )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	#get the value of i (if any)
        i <- x$getinv()
	
	#if the value of i already exists, then get it from cached data and skip calculation
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	
	#if not exists, calculate inverse
        data <- x$get()
        i <- solve(data, ...)
        
	#make cache
	x$setinv (i)
	## Return a matrix that is the inverse of 'x'
        i
        
}
