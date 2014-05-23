## makeCacheMatrix and cacheSolve work together to calculate the inverse of a matrix, store it
##  in the cache, and pull the calculation from the cache if needed instead of rerunning the 
##  calculation

## used https://class.coursera.org/rprog-003/forum/thread?thread_id=650 for test cases


## makeCacheMatrix defines 4 functions: set, get, setinverse, getinverse
##  set makes the matrix value available in the global environment as x and sets the value of 
##    m to NULL to indicate that the inverse has not yet been calculated and stored in the cache
##  get is a way to pull the matrix
##  setinverse saves the calculated inverse in the cache
##  getinverse pulls the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks to see if the inverse has already been calculated and stored in the cache
##  if so, it returns the "getting cached data" message and the inverse
##  if not, it calculates and returns the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
