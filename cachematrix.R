## author: chris phipps
## the two functions below are intended to be able to execute the solve(x) function over 
## a given matrix and cache the results, then make those results available to a user

## makeCacheMatrix = creates a  matrix object that caches the inverse of a given matrix
## I modified the code for makeVector given in the course README
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}




## cacheSolve = computes the inverse of the special "matrix" returned by makeCacheMatrix
## I modified the code for cachemean given in the course README
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- solve(x)
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}


## I used the two example below to doublecheck the code above. Both are simple matrices, but they 
##allow me a quick check of the functions above.

## EXAMPLE 1
### Original Example 1 matrix came from http://www.r-tutor.com/r-introduction/matrix/matrix-construction,
### but that wasn't square, so I added a few numbers to give me a 3x3 matrix. thsi will be my test matrix

### myMatrix <- matrix(c(2,4,3,1,5,7,3,3,6), nrow=3, ncol=3)
### myMatrix
### [,1] [,2] [,3]
### [1,]    2    1    3
### [2,]    4    5    3
### [3,]    3    7    6
### > solve(myMatrix)
### [,1]        [,2]       [,3]
### [1,]  0.2142857  0.35714286 -0.2857143
### [2,] -0.3571429  0.07142857  0.1428571
### [3,]  0.3095238 -0.26190476  0.1428571


## EXAMPLE 2
### > my9Matrix <- matrix(c(4,4,3,2,6,9,3,3,6), nrow=3, ncol=3)
### > my9Matrix
### [,1] [,2] [,3]
### [1,]    4    2    3
### [2,]    4    6    3
### [3,]    3    9    6
### > cacheSolve(my9Matrix)
### getting cached data
### [,1]  [,2]       [,3]
### [1,]  0.15  0.25 -0.2000000
### [2,] -0.25  0.25  0.0000000
### [3,]  0.30 -0.50  0.2666667
