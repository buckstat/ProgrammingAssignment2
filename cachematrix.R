
# I basically took the template they gave us for Cacheing the mean
# and converted it using the Solve command for Matrices
#It took me quite awhile to understand what was going on, someone finally
#suggested using the browser() command and I started seeing what
#was going on, I'm still a little iffy about how the check for inverse works
# how does it parse all the matrices that have passed into the function,
# I'd love some feedback on that :)
# Use a command like this to execute the makeCacheMatrx function
#   a <- makeCacheMatrix(b) where b is a matrix then
#   cacheSolve(a) to get the Inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                              #set initial Values
  set <- function(y) {                     #intitialize and define the functions
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x                       # get the matrix
  setinv <- function(solve) inv <<- solve   # set the inverse value
  getinv <- function() inv                  # get the inverse value
  list(set = set, get = get,                #return the four  functions  
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  inv <- x$getinv()                         # get the inverse
  if(!is.null(inv)) {                       #if one exists return cached value
    message("getting cached data")          #
    return(inv)
  }
  data <- x$get()                           # otherwise compute the inverse
  inv <- solve(data, ...)
  x$setinv(inv)
  inv                                       # return the inverse
}

