## Two functions are created that will allow us to cache the inverse of an array. To facilitate calculations


## This function is used to create a special "matrix" object that can 
##cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  
  set <- function(g){
    x <<- g
    k <<- NULL
  }
  get <- function(){x
    }
  setInverse <- function(inverse) {
    k <<- inverse
  }
  getInverse <- function() {
    k
  }
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## Calculates the inverse of the special "matrix" created with the above function
## If the inverse has already been computed (and the matrix has not
## changed), then the "cacheSolve" should returned the inverse from 
##the cache and skip the computation. 
##Otherwise, it calculates the  inverse of the "matrix" and sets the value of the inverse in the cache 
##via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  k <- x$getInverse()
  if(!is.null(k)){
    message("getting cached data")
    return(k)
  }
  mat <- x$get()
  k <- solve(mat,...)
  x$setInverse(k)
  k
}
