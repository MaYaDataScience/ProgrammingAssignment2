## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly. 
 
## function to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<-NULL
	}
	get<- function()x
		setInv<-function(solve) m<<- solve
		getInv<-function() m
		list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Function to call cache or compute inverse of matrix with solve()

cacheSolve <- function(x, ...) {
	m <- x$getInv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInv(m)
	m

}
