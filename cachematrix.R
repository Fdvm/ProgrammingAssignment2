## The first function will create a list where we can store in a cache the values 
## of an inverted matrix. The second will look for this values in the cacha and
## return them

## The function defines 4 diferent functions which are saved in a list (set,
## get, setinverse and getinverse). The first sets a value for the matrix, the
## second gets the value, the third sets the value of the inverted matrix and the
## lastest gets the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function first called "getinverse" function, which was stored in a list
## with makeCacheMatrix function, and saves it as inv variable. After that, it
## evaluates if inv isn't NULL. If it isn't then it will return the inverted matrix,
## if not, it will call the matrix value from the list and calculate the interted
## matrix and set it as a new value in the list

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv  
}
