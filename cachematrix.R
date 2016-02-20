## Functions to create special matrices that cache their inverse

## Creates a matrix that can cache its inverse
makeCacheMatrix <- function(A = matrix()) {
	
	cachedInverse <- NULL 			# initialize cached inverse to NULL, since we don't know it yet.
	
	set <- function(B) {
		A <<- B 						# set this matrix (A) to new value (B) 
		cachedInverse <<- NULL		# reset cached inverse, since it is no longer valid
	}
	
	get <- function() A 				# just return this matrix (A)
	
	setInv <- function(newInv) {
		cachedInverse <<- newInv 	# sets cached inverse to new value
	}
	
	getInv <- function() {
		cachedInverse 				# just return the current cached inverse value
	}
	
	list(set = set, 
		get = get, 
		setInv = setInv, 
		getInv = getInv)
}


## Returns the inverse of M, a cached matrix
cacheSolve <- function(M, ...) {
        
    # if getInv() is null, then there is no cached inverse and we have to make it
	if (is.null(M$getInv())) {
		
		message("Please hold. Must cache a new inverse")
		
		# use solve to compute inverse of M (M$get()). Cache new inverse using M$setInv(). 
		# pass along variable arguments to solve function
		M$setInv(solve(M$get(), ...)) 
	}
	
	# return inverse value
	M$getInv()

}

