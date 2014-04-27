## Two functions performing caching of the inverse of matrix, use round(a %*% b) to check better for the multiplication of matrix and its inverse.

## makeCacheMatrix : it takes an invertible matrix as the parameter 'x' 
## 					 and returns a list wrapping 4 functions in it. It creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		mat <- NULL
		set<- function(y){
			x <<- y
			mat <<-NULL
		}
		get <- function() x
		setInverse <- function(solve)  mat<<-solve
		getInverse <- function() mat
		list(set =set, get=get,
			setInverse = setInverse,
			getInverse = getInverse)

}

## cacheSolve : it requires a "special matrix" made by makeCacheMatrix
##				The output is the inverse matrix, which is obtained either from the "special matrix's" cache or by calculation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		mat <- x$getInverse()
		if(!is.null(mat)){
			message("getting cached inverse matrix")
			return(mat)
		}
		data <- x$get()
		mat <- solve(data, ...)
		x$setInverse(mat)
		mat
		
}

