

# The code is written to store and evaluate the inverse of a matrix

# Call a memory in space matrix to hold the same

makeCachematrix <-  function(x=matrix())
    
{
    
    # Identify the Inverse Matrix as inv and call it as null
    # Identify the set variable as a function to hold y ultimately under a scoping argument  (Closure Functions)
    
    inv <- NULL
    set <-function(y)
    {
        x<<-y
        inv<<-NULL
        
    }
    # Set and define functions to get and set initialize the inverse of matrix
    # Defne a list contaiting the set , get , setinverse, getinverse function
    
    get <-function(){x}
    setinverse <-function(inverse) {inv <<-inverse}
    getinverse <-function() {inv}
    list(set = set, get= get,setinverse=setinverse,getinverse=getinverse )
    
}    

# Defne a function cacheSolve comprising of x (matrix); call the inv defined earlier to define / get the inverse using getinverse defined earlier. In case the inv is null, message out "Getting Cache Data" else return inv
# Store the get under mat variable and use solve to calculate the inv; now store / set the inverse using setinverse defined earlier


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting Cache Data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv    
    
    
}


            ############### Codes with description to run and evaluate the answers #####################


# Run the following commands to validate the store and inverse output of a matrix defined under pmatrix

#source("makeCachematrix.R")


# A matrix of 2 by 2 with elements 1-4 has been taken

#pmatrix <- makeCachematrix(matrix(1:4, nrow=2, ncol = 2))


# By calling get we store the function x ultimately denoting the matrix defined above

#pmatrix$get()
#cacheSolve(pmatrix)


# Finally call the cacheSolve with function x = pmatrix as and evaluate the inverse via getinverse

#pmatrix$getinverse()

