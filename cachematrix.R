

## This function takes a vector (will need to be ready to accept vector that can be converted to square matrix - I didn't write the error message)
## and then will set that vector as a matrix, and create a free floating variable, cache the matrix, and create a vector of functions
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL     
        set <- function(y) {  # this creates a set function which sets y = x and m = NULL
                x <<- y
                m <<- NULL
        }
        get <- function() x   # the get function returns the value stored for x
        setmatrix  <- function(solve) m <<- solve  # the setmatrix function has a forma input variable "solve" which sets that value = m, and then sets the value Solve in the  parent environment
        getmatrix <- function() m  # the getmatrix function returns the value of m
        list(set=set, get=get,     # this creates a list vector of functions
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function will take as input the above stored function (and it's cached matrix), 
## use the stored functions in the function environment and determine if the matrix exists
## and if it doesn't then get the matrix and solve it for the inverse of the matrix
## and finally return the invese of the matrix as output. 

cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()  # this determins if the matrix exists - this is T of F (null, not null)
        if(!is.null(m)) {   # if it does exist, then return the text and the value
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()     # if the matrix does not yest exist, first return the matrix
        m <- solve(matrix, ...)  # using the solve function, calculate the inverse of the matrix
        x$setmatrix(m)      # cache the matrix as m
        m                   # print m
}
