
## Assignment for lexical scoping
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL  ## Sets initial value of m to NULL
  set<-function(y){ ## sets the value of the matrix
  x<<-y  ## stores the matrix which cacheSolve can use later
  m<<-NULL ## sets value of matrix inverse to NULL is cacheSolve got used
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get, ## This list keeps track of the new functions
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
	## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix() ## Run the function to get the value of the input matrix
    ## Check to see if m is null; if it is NOT null
    ## then it will retrieve from the stored cache
    if(!is.null(m)){
      message("Retrieving data from stored cache")
      return(m)
    }
    ## Else, it will solve the inverse 
    matrix<-x$get()
    m<-solve(matrix, ...)
    ## And then then store it for later retrieval
    x$setmatrix(m)
    ## and finally output the inverse to the user
    m
}