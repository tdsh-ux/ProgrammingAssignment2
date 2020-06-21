makeCacheMatrix <- function(x = matrix()) {   #Create a list of special functions.
	i <- NULL   #Define the value of i.
	set <- function(y){   
		x <<- y   #Put y in the parent environment.
		i <<- NULL         #Set the value of i to NULL.
	}
	get <- function() x   #Get eht value of x (i.e., the matrix)
	setinverse <- function(inverse) i <<- inverse   #Set the value of the inverse in the parent environment.
	getinverse <- function() i   #Get the inverse (i).
	list(set = set, get = get,   #Named list.
		setinverse = setinverse, 
		getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()   #Atributtes the value of the inverse of x (possibly NULL).
        if(!is.null(i)) {   #If i is not NULL, then takes the stored value.
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()   #Otherwise, it computes the inverse using the solve() function (and returns it).
        i <- solve(data)
        x$setinverse(i)
        i
}
