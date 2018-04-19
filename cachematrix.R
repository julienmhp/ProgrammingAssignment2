## This double-function file reads a matrix, returns it's inverse and registers this value in cache memory

##  The first function intakes a matrix (1); it then saves the inverse to cache (3).

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y) {
		x<<-y
		inv<<-NULL
	}
	get<-function() x
	setinv<-function(solve) inv <<- solve
	getinv<-function() inv
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## The second function calculates the input matrix's inverse (2); it then returns the cache value of the matrix's inverse.

cacheSolve <- function(x, ...) {
	inv<-x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data<-x$get()
	inv<-solve(data,...)
	x$setinv(inv)
	inv
}


## a<-makeCacheMatrix(matrix(c(4,2,7,6),2,2))
## cacheSolce(a)