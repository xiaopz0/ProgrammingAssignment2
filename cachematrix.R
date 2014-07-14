## this function calculates and cache the inverse of a matrix
## 

## iniate a matrix object with an array object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv
  getinverse <- function() inv
  list(set= set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse
    )
}


## solve the inverse of a matrix and cache the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  return(inv)
}
