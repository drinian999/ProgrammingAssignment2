## LIke the example functions, these functions first prepare a matrix and cache it,
## then using the cached matrix--if available--invert it.

## Like the example makeVector, this function returns a list of functions
## that will be used in the cacheSolve function in order to enhance computing speed
## by utilizing previously calculated values.  It also takes account of the lexical scoping
## rules of R by using the << operator.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # inv is a placeholder for the eventual matrix inverse, like m was the placeholder
                    # for the eventual mean in the makeVector example. This operation nulls out any
                    # previous value that was stored in it.
        # now we define the function "set" ; as pointed out in the discussion forum by Joseph White
        # and Ezio Crestaz this function is not strictly needed for this exercise but is good 
        # programming practice to allow resetting of a previously existing object
        set <- function (y) {
                x <<- y        # x is replaced by y and now available in the global environment
                inv <<- NULL   # inv is made NULL in the global environment
        }
        get <- function() x    # gets x
        setinverse <- function(inverse) inv <<- inverse # sets inv in global environment to "inverse"
        getinverse <- function() inv                    # gets inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # stores the 4 functionss in MakeCAcheMatrix
}


## This function takes the prepared cached matrix from the previous function and inverts it

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()   #gets the getinverse matrix from the makeCacheMatrix
        if(!is.null(inv)) {     #gets cached inverse if it it's not NULL
                message("getting cached inverse matrix")
                return(inv)     # returns the cached inverse matrix and exits the function
                
        }
        data  <- x$get()        # if inv is NULL proceeds to calculate the inverse
        inv <- solve(data,...)  # generates the inverse and sets it to "inv"
        x$setinverse(inv)       # using the setinverse function from makeCacheMatrix set inv to the inverted matrix
}
## verified the code using the following example:
## created a 2x2 matrix: x <- matrix(c(5, -7, 2, -3))
## ran code: a <- makeCacheMatrix(x)
## ran a$get() and found my 2x2 matrix there: 
##  5  2
## -7 -3
## ran code cacheSolve(a): no response since first pass if condition not satisfied
## repeated; this time it printed my inverted matrix:
##  3  2
## -7 -5
