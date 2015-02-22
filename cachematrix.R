## These 2 functions are for getting the matrix that we want to calculate the inverse and keep the output
## when we need the value of it again, we don't need to re-calculate, just call back the value
## from cacheSolve function.


## makeCacheMatrix function use for creating a matrix that we want to calculate the inverse
## There are 4 sub-functions, set/get/solvematrix/getmatrix
makeCacheMatrix <- function(x = matrix()) {
    mtx <- NULL 
    set <- function(y) { ## set the new value 
        x <<- y
        mtx <<- NULL
       }
    get <- function() x
    solvematrix <- function(inputM) mtx <<- inputM
    getmatrix <- function() mtx
    list(set=set,get=get,solvematrix=solvematrix,getmatrix=getmatrix)
}

## cacheSolve function use for recall or calculate the matrix that gets from makeCacheMatrix
cacheSolve <- function(x, ...) {
  mtx <- x$getmatrix()
  if(!is.null(mtx)) {
    message("getting cached data")
    return(mtx)
    }
  data <- x$get()
  mtx <- solve(data)
  x$solvematrix(mtx)
  mtx
}
