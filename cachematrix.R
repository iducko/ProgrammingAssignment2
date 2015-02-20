
##I am trying to write an R function that uses R's scoping rules to
##preserve state inside of an R object.
##I will create two functions to create a special object that 
#     1) stores a matrix
#             and 
#     2) caches its inverse



##The first function, makeCacheMAtrix creates a special "matrix"
#which is really a list containing a function to
#1. Set the value of the matrix
#2. Get the value of the matrix
#3. Set the value of the inverse
#4. Get the value of the inverse

#Begin function here
#create the special matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
#set the value of the matrix
        set <- function (y){
          x <<- y
          m <<- NULL
        }
#get the value of the matrix
        get <- function() x
#set the value of the inverse, using the solve() function
        setmatrix <- function(solve) m <<- solve
#get the value of the inverse
        getmatrix <- function() m
        list (set = set, get = get,
                setmatrix = setmatrix,
                getmatrix = getmatrix)
}



#This second function calculates the inverse of the special "matrix" created with the
#makeCacheMatrix function above.
#1. It must first check to see if the inverse has already been calcuated
#2. If so, it gets the inverse from the cache and skips the computation
#3. If not, it calculates the inverse of the matrix
#and 4. sets the value of the inverse in the cache via the 'setmatrix' function

#Begin second function here
cacheSolve <- function(x, ...) {
#first check to see if the inverse has already been calculated
        m <- x$getmatrix()
#if so, write a note to say "getting cached data"
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
#if not, calculate the inverse of the matrix
        data <- x$get()
        m <- solve(data,...)
#and set the value of the inverse in the cache via the 'setmatrix' function
        x$setmatrix(m)
#Return a matrix that is the inverse of 'x'
        m
}
