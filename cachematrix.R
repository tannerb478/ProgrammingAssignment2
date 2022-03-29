## Put comments here that give an overall description of what your
## functions do

## Initializes the script by clearing the cache (mat.x.inv), creates the list of setter and getter functions for the
## solver. Sets the cached sollution mat.x.inv if it doesn't already exist and cacheSolve is run.

makeCacheMatrix <- function(x = matrix()) {
        mat.x.inv<-NULL   #this sets the cached matrix inverse to null
        set<-function(y){
                x<<-y             #assigns matrix to y in parent
                mat.x.inv<<-NULL  #sets mat.x.inv to null locally
        }
        get<-function() x         #retrieves matrix x from parent env
        set.inv<-function(solved.inv.x) mat.x.inv <<- solved.inv.x #sets the cached inverse, mat.x.inv to the solved value
        get.inv<-function() mat.x.inv  #retrieves cached inverse
        list(set = set, get = get, set.inv=set.inv, get.inv=get.inv)
}


## Checks to see if a cached inverse of the matrix exists. If so, returns cached solution and makes user aware.  
## If not, it solves for the inverse of the matrix and assigns the solution to "solved", which is then assigned
## to the cache using the set.inv function in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat.x.inv<-x$get.inv()
        if(!is.null(mat.x.inv)){
                message("getting cached inverse of matrix")
                return(mat.x.inv)
        }
        mat.x <- x$get()
        solved.inv.x <- solve(mat.x)
        x$set.inv(solved.inv.x)
        solved.inv.x
}
