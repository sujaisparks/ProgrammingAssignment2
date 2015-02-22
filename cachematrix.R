## makeCacheMatrix:
## This function creates an object that can cache the inverse of a matrix (the input paramter),
##      and returns a List of 4 functions as described below
## Within this fuction, a matrix object called invMatrix is created and initialized
## In addition few other functions are created viz., get, set, getInvMatrix, setInvMatrix
##      get: Returns the x matrix
##      set: defined to initialize x from outside function, but currently not used
##      getInvMatrix: Returns the inverse matrix
##      setInvMatrix: takes the 'inv' matrix object as input parameter and assigns to invMatrix

makeCacheMatrix <- function(x = matrix()) {
        
        #initialize inVMatrix to NULL
        invMatrix<-NULL
        
        #defining function set
        set<-function(y){
                x<<-y
                invMatrix<<-NULL
        }
        
        #defining function get
        get<-function() x
        
        #defining setInvMatrix
        setInvMatrix<-function(inv) invMatrix<<- inv
        
        #defining getInvMatrix
        getInvMatrix<-function() invMatrix
        
        #returns a list object containing the functions
        list(set=set,
             get=get,
             setInvMatrix=setInvMatrix,
             getInvMatrix=getInvMatrix)

}


## cacheSolve:
## This function takes the list as returned by function makeCacheMatrix 
##      and returns the inverse of a matrix (as cached by the makeCacheMatrix )
## Logic: 1. a check is made if the inverse matrix is already computed. If so, return the value
##      2. if inverse matrix is null, the variable matrix is assigned to the x matrix passed to makeCacheMatrix 
##      3. The inverse of the matrix is computed using 'solve' function and stored in invMatrix variable
##      4. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invMatrix<-x$getInvMatrix()
        #check if the inverse matrix has already been calculated
        if(!is.null(invMatrix)){
                message("inverse matrix has already been calculated..returning cached data")
                return(invMatrix)
        }
        
        #setting matrix to the matrix contained in makeCacheMatrix
        matrix<-x$get()
        
        #compute the inverse of the matrix
        invMatrix<-solve(matrix, ...)
        
        #set the inverse into the cache
        x$setInvMatrix(invMatrix)
        
        #get from cache and return
        return(x$getInvMatrix())
}
