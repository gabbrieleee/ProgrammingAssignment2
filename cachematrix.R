## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix consists of set, get, setInverse and getInverse

makeCacheMatrix <- function(x = matrix()){
   
   #initializing inverse as NULL
   inv <- NULL
   set <- function(y){
      x <<- y
      inv <<- NULL
   }
   #function to get matrix x
   get <- function() {x}
   #set value of inverse
   setInverse <- function(inverse) {inv <<- inverse}
   #get value of inverse
   getInverse <- function() {inv}
   
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}



##This function is used to get the cache data

cacheSolve <- function(x, ...) {
   inv <- x$getInverse()
   #checking whether inverse is NULL
   if(!is.null(inv)){
      message("getting cached data!")
      return(inv)
   }
   mat <- x$get()
   #calculates inverse value
   inv <- solve(mat, ...)
   x$setInverse(inv)
   #return a matrix that is the inverse of 'x'
   inv
}



#example:

#f <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))

#f$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#f$getInverse()
#NULL

#cacheSolve(f)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#cacheSolve(f)
#getting cached data!
#    [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#f$getInverse()
#    [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5



