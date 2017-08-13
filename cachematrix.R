## Cache Matrix - R Programming Week 3 - Assignment 2 - Lexical Scoping 
# English Is Not My Mother Tounge; Sorry If there are any mistake.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #Cache Holder
        #Clear the Cache
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x #Save Matrix For Functions To call
        setinv <- function(inv) m <<- inv #Function To Call For Caching Inv. Matrix
        getinv <- function() m #Inverse Matrix For Functions To Call
        #Naming FUnctions (get,setinv,getinv) to call later
        list(
                set = set,
                get = get,
                setinv = setinv,
                getinv = getinv
        )
}


cacheSolve <- function(x, ...) {
        inv <- x$getinv() #Get Inverse Matrix From Cache Function
        #Check If there is Inv. Matrix In the Cache Function
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #If There Is no Cache / Calculate Inverse Matrix
        data <- x$get() #Get Matrix / Ready for a Calculation
        inv <- solve(data, ...) #Calculate Inverse Matrix
        x$setinv(inv) #Take This Function's "inv" and Cache it
        inv #Show Inverse Matrix
}