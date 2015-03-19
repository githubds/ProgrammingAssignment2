## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
##makeCacheMatrix 
##	input param--1. a square matrix; returns a list of functions
##	this function creates an enviornment and initalizes x_inv variable
##	enclosing functions
##		set --assigns to variables in parent env
##		get --returns the input parameter of the parent function
##		setInv --sets value to parent env variable  x_inv
##		getInv --gets the parent env variable x_inv	
## cacheSolve 
##	inputparam --function; returns the inverse of square matrix
##		first it checks if the inverse is available in cache; if not then
##		it calculates inverse and stores that in the cache

makeCacheMatrix <- function(x = matrix()) {

	x_inv<-NULL

	set<- function(y){
		x<<-y
		x_inv<<-NULL
	}

	get<-function() x
	
	setInv<- function(inv)	x_inv<<-inv

	getInv<- function() x_inv
	
	list(	set=set
		,get=get
		,setInv=setInv
		,getInv=getInv
		)
	

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getInv();
	if (!is.null(m)){
		print("getting cached data...")
		return (m)
	}

	z<-x$get()
	m<-solve(z)
	x$setInv(m)

	m

}