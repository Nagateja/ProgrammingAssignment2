## Put comments here that give an overall description of what your
## functions do

##For readability, sentences have not been written in a single line.
##Check the next line for continuity.

##makeCacheMatrix is essentially stores a list of functions (set, get, 
##setinverse and getinverse). These functions can be accessed from the
##makeCacheMatrix function by command x$set or x$getinverse etc.
##set is used to set the value of the vector
##get is used to get the value of the vector
##setinverse is used to set the value of the inverse vector
##getinverse is used to set the value of the inverse vector
##<<- used in this function is useful to assign value to a variable outside
##of its environment
##E.g. x<<-y is used to set the value of x to y. Similarly i<<-NULL sets the
##value of i to NULL outside of i environment.
##get<-function() x is returning the value x.
##At the end of the function, a list of functions (set etc.) is returned.

##cacheSolve is essentially returning the inverse of a matrix x if it has
##not been evaluated before. It returns the cache value (value that has been
##saved in memory temporarily) if the value has been computed already.
##x$getinverse function assigns the inverse of the matrix to i
##The if block of the function checks to see if value of i is not null.
##if not null(i.e. inverse already computed), then a message "getting
##cached data" is printed and the value of i is returned.
##If i is null, then inverse of the matrix is computed using solve()
##solve(data,...) is assigned to i and i is returned at the end of function.

## Write a short comment describing this function
##Stores a list of functions (set, get, setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
set <- function(y){
  x<<-y
  i<<-NULL
}
get <- function()x
setinverse <- function(inverse) i<<-inverse
getinverse<-function() i
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##Returns the cache of the matrix x if the inverse is already computed

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
   data<-x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i      ## Return a matrix that is the inverse of 'x'
}
