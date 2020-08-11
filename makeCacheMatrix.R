makeCacheMatrix <- function(x = matrix()) {                     ##1st function Start#Create makeCachematrix as a new function where x is a matrix
        inv <- NULL				                ## Define variable inv
        set <- function(y) {                                    ##1Set the value of the matrix. Create set as a new function where y is assigned to x
                x <<- y 			        	##Set the value of the matrix using another function(y)
                inv <<- NULL		                	##Double arrow <<- is capable to be modify variable in parent environ while <- only allow to modify within current environ
        }

        get <- function() {x}                                   ##2Get the value value of the matrix. Create get as a new function with no argument. x is the body ##Outside the set function, we will get the value of the matrix
        setInverse <- function(inverse) {inv <<- inverse}       ##3Set the value of the inverse of x
        getInverse <- function() {inv}                          ##4Get the value of the inverse of x
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}				                        	##1st function End

cacheSolve <- function(x, ...) {			        ##2nd function Start
        inv <- x$getInverse()                                   ##return a matrix that is inverse of x, and asisgn to inv
        if(!is.null(inv)) {
                message("getting cached data")                  ##If inverse is going to be retrieve from the cache, then you will see "getting cached data" displayed
                return(inv)		                        ##Inverse will be return
        }
        mat <- x$get()			                        ##Otherwise, it will be required to compute the matrix and set inverse in the cache
        inv <- solve(mat, ...)		                        ##To compute the inverse use solve standard R function
        x$setInverse(inv)			                ##Now is time to save the value of the inverse into cache so use setinverse function
        inv
}
