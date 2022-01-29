
## makeMatrix
## returns a special "matrix", which is a list of functions that:
## sets the value of matrix, gets the value of the matrix, 
## sets the value of the inverse, gets the value of the inverse.
## input is a numeric matrix
makeMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve()
## take the output of makeMatrix()
## calculates and caches the inverse
## cancall it as cacheSolve(makeMatrix(my_matrix))
cacheSolve <- function(x, ...) {
  m <- NULL
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m 
}



