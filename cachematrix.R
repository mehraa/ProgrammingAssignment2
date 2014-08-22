#The 2 functions below are used to cache the inverse value of a given matrix 'x' 
#as a value 'm' in another environment so as to reduce the time taken in 
#time-consuming calculations

#makeCacheMatrix defines and lists the 4 separate functions used in the next function.
makeCacheMatrix <- function(x = matrix()) #x is a matrix 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve  #find inverse of 'x' and cache it
  getinverse <- function() m     #get cached inverse value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)   #4 defined functions
}


#cacheSolve returns the inverse of a given matrix 'x', either from the cache, or by 
#calculating it using the solve function.
cacheSolve <- function(x, ...) {  
  m <- x$getinverse() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)                 #return m if available in cache
  }
  data <- x$get()
  m <- solve(data, ...)         #solve for 'x' whose inverse is not available in cache
  x$setinverse(m)               #cache the inverse matrix
  m
}