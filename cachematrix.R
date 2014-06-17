## Testing: 
## B = matrix(runif(16),4)
## O = makeCacheMatrix(B)
## O$set_result(NULL)
## D = cacheSolve(O)
## C = D %*% B
## check <- 1
## for (cnt in 1:length(C[1,])) {check <- check * C[cnt,cnt] }
## check <- round(check, digits = 5)
## if (check != 1) { warning('Bad inverse')}
## if (is.null(O$get_result())){ warning('Bad cache usage')}

## Write a short comment describing this function

makeCacheMatrix <- function(simple_matrix = matrix()) {
cached_result <- NULL
set <- function(input) {
	simple_matrix <<- input
	cached_result <<- NULL
}
get <- function() simple_matrix
set_result <- function(result) cached_result <<- result
get_result <- function() cached_result
list(set = set, get = get, set_result = set_result, get_result = get_result) 
}


## 

cacheSolve <- function(x, ...) {
m <- x$get_result()
if(!is.null(m)) {
	message('Getting cached data')
	return(m)
} else {
	message('Solving the matrix')
}
data <- x$get()
m <- solve(data, ...)
x$set_result(m)
m
}
