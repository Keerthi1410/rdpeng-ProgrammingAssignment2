# rdpeng-ProgrammingAssignment2

##MakeCacheMatrix
##Function to compute inverse of matrix

> makeCacheMatrix <- function(x = matrix()) {
+     inv <- NULL
+     set <- function(y) {
+         x <<- y
+         inv <<- NULL
+     }
+     get <- function() x
+     setinv <- function(inverse) inv <<- inverse
+     getinv <- function() inv
+     list(set = set, get = get, setinv = setinv, getinv = getinv)
+ }
> 
> cacheSolve <- function(x, ...) {
+     inv <- x$getinv()
+     if(!is.null(inv)) {
+         message("getting cached result")
+         return(inv)
+     }
+     data <- x$get()
+     inv <- solve(data, ...)
+     x$setinv(inv)
+     inv
+ }	

##Validation
> mat <- matrix(rnorm(20),4,4)
> mat1 <- makeCacheMatrix(mat)
> cacheSolve(mat1)
           [,1]       [,2]        [,3]       [,4]
[1,] -1.0009295  0.1803799 -0.31323756 -0.7089995
[2,] -0.4688305  0.4297353  0.32602573 -0.9481825
[3,] -0.7462761 -0.2687447  0.01514044 -0.8092491
[4,]  2.1886045  2.0070891 -1.17287765 -1.6180329
