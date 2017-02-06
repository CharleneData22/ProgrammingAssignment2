## The makeCacheMatrix creates a special a list containing a function to 
##     1) set the value of the matrix
##     2) get the value of the matrix
##     3) set the value of the inverse matrix
##     4) get the value of the inverse matrix
## The cacheSolve function calculates the inverse matrix of the matrix the created with the above function. 
##     However, it first chek to see if the inverse matrix has already been calculated. 
##     If so, it gets the inveres matrix from the cache nd skips the computation.
##     Otherwise, it calculates the inverse matrix of the original matrix (omatrix) 
##        and sets the inverse matrix in the cache via the setinverse function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL			#making initial I value as null
        set <- function(y){
                x <<- y			#sets makeCacheMatrix x
                i <<- NULL		# sets makeCacheMatrix i back to null, when x is set
        }
        get <- function() x		# get makeCacheMatrix x value 
        setinverse <- function(inverse) 
                i <<- inverse	        #set makeCacheMatrix I from the inverse input (cacheSolve i <- solve(data)
        getinverse <- function() i	# get makeCacheMatrix i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        i <- x$getinverse()	# makeCacheMatrix i
        if(!is.null(i)) {       # when I already has meanful value cached from the last computerion
                message("getting chached data")
                return(i)	# return the i from cache
        }
        omatrix <- x$get()		# when i is null
        i <- solve(omatrix, ...)	# make i as inverse matrix of x
        x$setinverse(i)		# set the just computed i to the makeCacheMatrix i
        i			## Return a matrix that is the inverse of 'x'
}
