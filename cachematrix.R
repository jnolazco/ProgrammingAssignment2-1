############
# This function creates a special "matrix" object that can cache its inverse, which is really a list containing 
# a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      invMat <- NULL
      set <- function(y) {                      # 1. set the value of the matrix
            x <<- y
            invMat <<- NULL
      }
      get <- function() x                       # 2. get the value of the matrix
      setinverse <- function(inverseMatrix) invMat <<- inverse # 3. set the value of the matrix 
                                                               # in the cache
      getinverse <- function() invMat                          # 4. get the value of the matrix
      list( set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

############
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invMat <- x$getinverse()
      if(!is.null(invMat)) {                          # if inverse matrix has been already calculated
            message("getting cached data")
            return(invMat)                            # get the inverse matrix from the cache.
      }
      data <- x$get()                                 # if not, it gets the data
      invMat <- solve(data, ...)                      # calculate the inverse matrix of the data.
      x$setinverse(invMat)                            # set the value of the inverse matrix in the cache.
      invMat                                          ## Return a matrix that is the inverse of 'x'
}
