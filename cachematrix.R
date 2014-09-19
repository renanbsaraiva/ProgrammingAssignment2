##this function creates a the inverse of a matrix in the cache and retrieves it when necessary
##Also, the function creates a object "m" which stores the inverse of a matrix x in the cache


makeCacheMatrix <- function(x = matrix()){  
  m <- NULL       #set variable m to NULL
  set <- function(y) {    #set function sets x to the argument y and set m to null
    x <<- y
    m <<- NULL
  }
  get <- function () x #get returns the value of m (argument of fuction makeCacheMatrix)
  setinvmatrix <- function(solve) m <<- solve #setinvmatrix creates the inverse of a matrix and assign it to m
  getinvmatrix <- function() m #getinvmatrix gets the inverted matrix "m" created in setinvmatrix
  list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix) #returns a labeled vector of functions set,get,setmean and getmean
}


## This function returns the inverse of a matrix "x". If the
## inverse of "x" has already been calculated and stored 
## in the cache, the function returns the stored inverse of "x"


cacheSolve <- function(x, ...){ #attempts to get the matrix from x (if it was already created)
  m <- x$getinv()
  if(!is.null(m)) {     #if not null, a value was cached, so just return m
    message("getting cache data")
    return(m)
  }    #if it is null, set data to x from makeCacheMatrix
  data <- x$get()
  m <- solve(data, ...)    #create a inverted matrix of data
  x$setinvmatrix(m)     #set m in x to created matrix
  m   #return solved matrix
}
