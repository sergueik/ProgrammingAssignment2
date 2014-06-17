## function for object with internal data 
  
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

