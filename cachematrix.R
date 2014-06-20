## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {                #the set function can store the data
            x <<- y
            inv <<- NULL
      }
      get <- function() x                 #the get function can retrieve the stored data
      setinv <- function(inv) m <<- inv   #the setinv function can set the inverse of the matrix
      getinv <- function() inv            #the getinv function can retrieve the inverse of the matrix
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      inv <- x$getinv()                   #get the stored inverse
      if(!is.null(inv)) {                 #if the inverse already exists, return the cached inverse
            message("getting cached data")
            return(inv)
      }
      data <- x$get()                     #if the inverse does not exist, then calculate the
      inv <- solve(data, ...)             #inverse of matrix using solve function
      x$setinv(inv)                       #store the computed inverse
      inv                                 #return the inverse of the matrix
}



