#' @method makeCacheMatrix
#' @details Matrix inversion is usually a costly computation and there may be some 
#' benefit to caching the inverse of a matrix rather than compute it repeatedly
#'
#' This function creates a matrix that can cache the inverse of matrix
#' 
#' @param x Square matrix that is invertible
#' @return Returns a list containing methods to manage inverse cacheing matrix
#' 
#' Inner Methods
#' @method set(x) Sets an new invertible matrix if required
#' @method get(x) Returns the original matrix
#' @method setinv(x) Sets the inverse of the matrix
#' @method getinv() Returns/Gets the Inverse of the matrix
#' 
#' @usage A <- matrix( c(5, 1, 0,
#'                       3,-1, 2,
#'                       4, 0,-1), nrow=3, byrow=TRUE)
#' cA <- makeCacheMatrix (A)                       

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function () x
    setinv <- function (mat.inv) inv <<- mat.inv
    getinv <- function () inv
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


#' @method cacheSolve(x, ...) Solve the given matrix by creating its inverse. If 
#' the inverse is already created, the cache'd inverse is returned
#' @param x A cacheable matrix created by calling makeCacheMatrix
#' 
#' @return Returns the inverse of the Cache'd Matrix 
#' @usage cacheSolve(cA)

cacheSolve <- function(x, ...) {
    mat.inv <- x$getinv()
    if (!is.null(mat.inv)){
        message ("getting cached data")
        return (mat.inv)
    }
    
    # Get the original matrix data and solve it
    mat.inv <- solve(x$get())
    
    # Set the Inverse matrix for later use
    x$setinv(mat.inv)
    
    # Return the inverse
    mat.inv
}


# Example run
# A <- matrix( c(5, 1, 0,
#                3,-1, 2,
#                4, 0,-1), nrow=3, byrow=TRUE)
# cA <- makeCacheMatrix(A)
# cA$get()
## [,1] [,2] [,3]
## [1,]    5    1    0
## [2,]    3   -1    2
## [3,]    4    0   -1

# B <- cacheSolve(cA)
# B
## [,1]    [,2]   [,3]
## [1,] 0.0625  0.0625  0.125
## [2,] 0.6875 -0.3125 -0.625
## [3,] 0.2500  0.2500 -0.500

# Note: If we call cacheSolve(cA) again, we get the Cached data
# cacheSolve(cA)
## getting cached data
## [,1]    [,2]   [,3]
## [1,] 0.0625  0.0625  0.125
## [2,] 0.6875 -0.3125 -0.625
## [3,] 0.2500  0.2500 -0.500

# Checking the Inverse -- We should get back A
# cB <- makeCacheMatrix(B)
# cacheSolve(cB)
# [,1] [,2] [,3]
# [1,]    5    1    0
# [2,]    3   -1    2
# [3,]    4    0   -1

# Note that we got back the original matrix we had given.