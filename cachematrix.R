## Returns a list of four functions to store and retrieve the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(invm) m <<- invm
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## Returns inverse of Matrix 

cacheSolve <- function(x, ...) {
  
  
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data..")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}
