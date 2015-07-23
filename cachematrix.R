## These functions will compute and cache the inverse of a matrix

## Create a list with functions to:
#  1. set the values of the matrix
#  2. get the matrix
#  3. set the inverse matrix
#  4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      i<-NULL
      set<-function(y){
            x<<-y
            i<<-NULL
      }
      get<-function(){x}
      setinverse<-function(inverse){i<<-inverse}
      getinverse<-function(){i}
      list(set = set, get = get, 
           setinverse = setinverse,
           getinverse = getinverse)
}

## Compute the inverse of a matrix after it has been passed
#  to the makeCacheMatrix function above or retrieve the cached
#  inverse matrix if it has already been computed

cacheSolve <- function(x, ...) {
      i<<-x$getinverse()
      if (!is.null(i)){
            message("getting cached data")
            return(i)
      }
      data<-x$get()
      i<-solve(data,...)
      x$setinverse(i)
      i
        ## Return a matrix that is the inverse of 'x'
}
