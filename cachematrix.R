# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setinv <- function(solve) {inv <<- solve}
  getinv <- function() {inv}
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x,...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

#Test 
#x <- matrix(rnorm(25), nrow = 5)
#inv = makeCacheMatrix(x)
#cacheSolve(inv)

#[,1]        [,2]       [,3]       [,4]        [,5]
#[1,]  0.5593267 -0.12295926 -1.0211661 -0.2757621 -0.42790410
#[2,]  0.3514782  0.57781756 -1.0897176 -0.6866044 -1.12533548
#[3,]  0.2444201  0.05135453 -1.1248228 -0.1415890 -0.05326594
#[4,]  0.2810643 -0.27521888 -1.9256970 -0.6227361 -0.42020235
#[5,] -0.1879845  0.18650195 -0.1007408  0.2408579 -0.37676859

# cacheSolve(inv)

#[,1]        [,2]       [,3]       [,4]        [,5]
#[1,]  0.5593267 -0.12295926 -1.0211661 -0.2757621 -0.42790410
#[2,]  0.3514782  0.57781756 -1.0897176 -0.6866044 -1.12533548
#[3,]  0.2444201  0.05135453 -1.1248228 -0.1415890 -0.05326594
#[4,]  0.2810643 -0.27521888 -1.9256970 -0.6227361 -0.42020235
#[5,] -0.1879845  0.18650195 -0.1007408  0.2408579 -0.37676859

#> cacheSolve(m)
#getting cached data
#[,1]        [,2]       [,3]       [,4]        [,5]
#[1,]  0.5593267 -0.12295926 -1.0211661 -0.2757621 -0.42790410
#[2,]  0.3514782  0.57781756 -1.0897176 -0.6866044 -1.12533548
#[3,]  0.2444201  0.05135453 -1.1248228 -0.1415890 -0.05326594
#[4,]  0.2810643 -0.27521888 -1.9256970 -0.6227361 -0.42020235
#[5,] -0.1879845  0.18650195 -0.1007408  0.2408579 -0.37676859
