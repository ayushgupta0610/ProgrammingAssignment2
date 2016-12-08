## makeCacheMatrix stores a calculated inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  #create a placeholder to store a calculated inverse
  Xinverse <- NULL
  #create a function that stores a new calculated inverse
  # in a new variable
  set <- function(y=matrix()) {
    #transfer the value of x to y
    x <<- y
    #store the inverse
    Xinverse <<- NULL
  }
  #create get function to return the matrix "x"
  get <- function() x
  #store the calculated inverse of "x": inversed is assigned to Xinverse
    setinverse <- function(solve) Xinverse <<- solve
    #create a function that returns the cached inverse, Xinverse
    getinverse <- function() Xinverse
    #create a list of matrices
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}


## cacheSolve function calls for the inverse of a matrix in the cache matrix
##if the cache doesn't have the inverse of the matrix, cacheSolve calculates
#the inverse of the matrix and then sends the result to the makecachematrix
#to be stored for later use

cacheSolve <- function(x, ...) {
  #check whether the inverse is in the cache
  Xinverse <- x$getinverse()
  #test if the cachedinverse has a stored value
  if(!is.null(Xinverse)){
        ## Return a matrix that is the inverse of 'x'
    return(Xinverse)
  }
  #if cachedinverse is null, then skip the if statement and calculate the inverse
  #call for the matrix and assign it to a variable, 
  data <- x$get()
  #assign solution to cachedinverse variable
  Xinverse <- solve(data)
  #call for setinverse function and store the the inverse of the matrix in cache
  x$setinverse(Xinverse)
  #return the inverse of the matrix
  Xinverse
  
}
#finalcommit

#sample data to run. Create a 2x2 invertible matrix
x <- matrix (1:4, 2,2)
#run the code
cacheSolve(makeCacheMatrix(x))
