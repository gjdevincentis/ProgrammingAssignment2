## Put comments here that give an overall description of what your
## functions do

## Creates special cached Matrix List
makeCacheMatrix <- function(x = matrix()) {
      ## sets inverse variable within environment
      inverse <- NULL
      
      ## defines get, set, get, setInverse, getInverse as funcions
      set <- function(y) {
            x <<- y
            inverse <- NULL
      }
      get <- function() x
      setInverse <- function(inv) inverse <<- inv 
      getInverse <- function() inverse
      
      ## assigns those functions to an outputed list
      list(set = set, get = get, 
           setInverse = setInverse,
           getInverse = getInverse)

}


## recalls cache, or calculates and caches inverse
cacheSolve <- function(x, ...) {
      inverse <- x$getInverse()
      ## tests if inverse is already cached
      if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
      }
      ## if not already cached, calculates, caches and returns inverse
      data <- x$get()
      inverse <- solve(data)
      x$setInverse(inverse)
      inverse
}

