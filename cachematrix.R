makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## creates an empty object
        set <- function(y) { ## create a function which.. 
                x <<- y ## assigns y (a value) to x...
                m <<- NULL ## and an empty object to m
        }
        get <- function() x ## returns x
        setSolve <- function(solve) m <<- solve  ## inverts the matrix
        getSolve <- function() m ## returns the inverted matrix
        list(set = set, get = get, ## creates a list of all four functions
             setSolve = setSolve,
             getSolve = getSolve)
}

## for proper use of the function, create a new object and assign this object the result of the previous function
## example: matr<-makeMatrix(here_your_matrix)
## then call cacheSolve(matr)

cacheSolve <- function(x, ...) { 
	m <- x$getSolve() ## assigns the inverted matrix to m

	if(!is.null(m)) { ## if m is not NULL i.e. was previosly calculated
		message("getting cached data") ## gives the message
		return(m) ## returns the inverted matrix
	}
	
	else{ ## if m is NULL
	data <- x$get() ## assigns the x value to a new object called data
	m <- solve(data, ...) ## inverts the matrix and assigns it to m
	x$setSolve(m) ## caches the newly inverted matrix  
	m ## returns the inverted matrix
	}
}
