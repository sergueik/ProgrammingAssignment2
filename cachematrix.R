## Testing: 
## B = matrix(runif(16),4)
## O = makeCacheMatrix(B)
## O$set_data(NULL)
## D = cacheSolve(O)
## C = D %*% B
## check <- 1
## for (cnt in 1:nrow(C)) {check <- check * C[cnt,cnt] }
## if (round(check, digits = 5) != 1) { warning('Bad inverse')}
## if (is.null(O$get_data())){ warning('Bad cache usage')}

## function for object with internal data 
## constructs functions to deal with the data object stored within
makeCacheMatrix <- function(self = matrix()) {
	data <- NULL
	list(set =  function(input) {
			self <<- input
			data <<- NULL
		},
	get = function()  self, 
	set_data = function(value) {
			data <<- value
		}, 
	get_data = function(){ 
			data 
		} 
	) 
}


## perform lengthy calculation and store the result in the sut
## by first trying to get result from the cache
## and if not available compute the result directly 
## and place it into the object's cache
cacheSolve <- function(x, ...) {
	result <- x$get_data()
	if(!is.null(result)) {
		message('Getting cached data')
		return(result)
	} else {
		message('Solving the matrix')
	}
	data <- x$get()
	result <- solve(data, ...)
	x$set_data(result)
	result
}

