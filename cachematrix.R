## makeCacheMatrix is a list that contains a few functions. You input a matrix
## and the functions interact with the matrix, mostly doing trivial things.

makeCacheMatrix <- function(x = matrix()) {
    
    #below resets the inverse to null, good practice to avoid confusion
    i <-NULL
    #set makes what we pass the object in focus. It only works if we pass a matrix (I think...)
    # it uses a <<- because it changes what x equals in the parent function (makeCacheMatrix)
    # Otherwise, x would only equal y in the set function.
    set <- function(y) {
        #!!!! set can make x not equal a matrix!!!!
        x <<- y
        #below resets the inverse in the Global Environment. Since it will be
        #calculated in the cacheSolve function, we have to use the <<-
        i <<- NULL
    }
    #get returns what our matrix is set to.
    get <- function() x
    #set Inverse returns our parameter i. i, in cacheSolve, is set to solve the inverse of our matrix.
    setInverse <- function(inverse) i <<- inverse
    #getInverse returns our i parameter
    getInverse <- function() i
    
    # below is a list of each function, this way we can call one of the objects 
    # (the four functions) using the object operator. For example:
    # a <- makeCacheMatrix(matrix(1:4, 2, 2))
    # a$get (will return 2 by 2 matrix with 1:4)
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)

}


## cacheSolve takes our parameter x, defined in the Global Environment by 
## makeCacheMatrix above, and checks if the inverse of it has been solved.
## If it has, it just returns it, if it hasnt, it calcuates it, and then returns it
##like makeCacheMatrix above, it is a list that contains numerous functions that
## are called using the object operator $

cacheSolve <- function(x, ...) {
    #the x$getInverse object below is defined in makeCacheMatrix above.
    # the line below sets i equal to that object.
    # x was defined in the Global Environment, thats why we can place it in this function
    i <- x$getInverse()
    #below checks if i is NULL
    if(!is.null(i)) {
        message("Getting cached data")
        #return(i) ends the function, so the below works like an else.
        return(i)
    }
    #below makes the parameter matrix equal the x$get() object defined in the global parameter
    matrix <- x$get()
    #below solves for the inverse of the matrix
    i <- solve(matrix)
    #below calles the setInverse function on x, and passes i as the parameter.
    # i is assigned in the Global Environment
    x$setInverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
