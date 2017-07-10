## Pair of functions that cache the inverse of a matrix.

## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

	##Initialize inverse
	m <- NULL

	##set function
	set <- function(y){
	    x <<- y
	    m <<- NULL
	}

	##get function
	get <- function() x

	##set inverse function
	setinverse <- function(inverse) m <<- inverse
	
	##get inverse function
	getinverse <- function() m 

	
	list( set = set, 
		get = get, 
		setinverse = setinverse, 
		getinverse = getinverse )

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##    then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

	##Get Inverse
	m <- x$getinverse()

	##Return Inverse if it is not NULL
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

	##get a cache matrix
	data <- x$get()

	## Calculate Inverse of Matrix
	m <- solve(data, ...)

	## Set/Cache inverse
	x$setinverse(m)

      ## Return a matrix that is the inverse of 'x'
	m       

}
