
# Call a memory in space matrix to hold the same

makeCachematrix <-  function(x=matrix())
    
{
    inv <- NULL
    set <-function(y)
    {
        x<<-y
        inv<<-NULL
        
    }
# Set and define functions to get and set initialize the inverse
        
        get <-function(){x}
        setinverse <-function(inverse) {inv <<-inverse}
        getinverse <-function() {inv}
        list(set = set, get= get,setinverse=setinverse,getinverse=getinverse )
        
}    
        cachemean <- function(x, ...) {
            inv <- x$getinverse()
            if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
            }
            mat <- x$get()
            inv <- solve(mat, ...)
            x$setinverse(inv)
            inv    
        
        
        }
        
        
        # Run the following commands to validate the store and inverse output of a matrix defined under pmatrix
        
        #source("makeCachematrix.R")
        #pmatrix <- makeCachematrix(matrix(1:4, nrow=2, ncol = 2))
        #pmatrix$get()
        #cachemean(pmatrix)
        #pmatrix$getinverse()
        
