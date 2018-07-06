##This program contains two major functions. The first (makeCacheMatrix) takes a matrix,
##      stores it in a variable, creates an empty object which will take the inverse of
##      that matrix, and is accessible from the global environment.
##The second (cacheSolve), takes a matrix, and checks whether it has had its inverse
##      previously solved and stored. If it has, it displays the inverse, if it has not,
##      it calculates the inverse, and stores the value in a variable shared by the first
##      and second functions. 


## makeCacheMatrix taxes a matrix, stores it,  and creates an empty value, 
##      which will later contain the inverse of that matrix. 
## It goes on to make both accessible in the global environment, and allows commands 
##      to alter or retrieve the matrix and its inverse from (cached) variables. 

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes a matrix that has been run through makeCacheMatrix, and either solves
##      for the inverse of that matrix, or if this has already been done, returns the
##      chached value of the inverse matrix variable and displays it, rather than 
##      re-calculating. 

cacheSolve <- function(x,...){
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        mymatrix <- x$get()
        m <- solve(mymatrix)
        x$setinverse(m)
        m
}