## makeCacheMatrix() creates an R object that will store the matrix and its inverse

# makeCacheMatrix() initializes x and inv objects in the beginning as a matrix and NULL, respectively
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        
        set <- function(y){
                x <<- y # assigns input argument (e.g., aMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))) to object x in parent environment; it may appear redundant but this argument can be used to just assign a new value of x in an already existing makeCacheMatrix() object
                inv <<- NULL # assigns NULL object to inv object in parent environment; clears any value of inv that has been cached in cacheSolve
        }
        
        get <- function() {x} # retrieve x object from parent environment of makeCacheMatrix
        setinv <- function(inv_matrix) {inv <<- inv_matrix} # since inv already defined in parent environment and it needs to be accessed after setinv is completed, "solve" input argument assigned to inv object in parent environment
        getinv <- function() {inv} # retrieve the inv object
        
        # assign a list of the functions and objects to the parent environment
        list(set = set, # gives the name 'set' to the set() function defined above
             get = get, # gives the name 'get' to the get() function defined above
             setinv = setinv, # gives the name 'setinv' to the setinv() function defined above
             getinv = getinv) # gives the name 'getinv' to the getinv() function defined above
        
        
        
}


## cacheSolve() will populate the inverse of the matrix by using the nested solve() function from the object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv() # attempt to retrieve inverse matrix from the object passed into cacheSolve() (e.g., cacheSolve(aMatrix)) as an argument
        if(!is.null(inv)){ # since makeCacheMatrix() sets the inv object to NULL for any new matrix, if inv is not NULL, cached data will be returned 
                message("getting cached data")
                return(inv)
        }
        
        # if the inv object is NULL, then cacheSolve gets the matrix, calculates its inverse, uses setinv() to set the inv object and then returns that         calculated inverse matrix into the parent environment
        mat <- x$get() # 
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
        
}
