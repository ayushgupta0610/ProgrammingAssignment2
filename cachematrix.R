## Two functions performing caching of the inverse of matrix, use round(a %*% b) to check better for the multiplication of matrix and its inverse.

## makeCacheMatrix : it takes an invertible matrix as the parameter 'x' 
## 					 and returns a list wrapping 4 functions in it. It creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(outputMatrix = matrix()) {
        cacheMatrix <- matrix();
        set <- function(matrixToBeSet) {
                outputMatrix <<- matrixToBeSet;
                cacheMatrix <<- matrix();
        }
        get <- function() outputMatrix
        setInverse <- function(Matrix) cacheMatrix <<- Matrix
        getInverse <- function() cacheMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse);
}

## cacheSolve : it requires a "special matrix" made by makeCacheMatrix
##				The output is the inverse matrix, which is obtained either from the "special matrix's" cache or by calculation. 

cacheSolve <- function(x, ...) {
        cacheMatrix <- x$getInverse();
        if(!is.na(cacheMatrix)) {
                message("getting cached inverse matrix");
                return(cacheMatrix);
        }
        outputMatrix <- x$get();
        Matrix <- solve(outputMatrix, ...);
        x$setInverse(Matrix);
        cacheMatrix;
}

#sample data to run. Create a 2x2 invertible matrix
x <- matrix (1:4, 2,2)
#run the code
cacheSolve(makeCacheMatrix(x))
