## The idea was to create a function that can cache inverse of the matrix.
## So, it has been done.You can see the results below. 

## In order to perform this task, R 4.0.3 (2020-10-10) "Bunny-Wunnies Freak Out"
## has been used.

## I hope the work is done correctly.
## And, of course, I take any constructive criticism positively.
## Thanks!


## The following function will create matrix which can cache its inverse:

makeCacheMatrix <- function(mtx = matrix()) {
        
        inv <- NULL
        set <- function(matrix) {
                mtx <<- matrix
                inv <<- NULL
        }
        get <- function() {
                mtx
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        getInverse <- function() {
                inv
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
       
}


## Next we will compute inverse of the matrix returned by the 
## makeCacheMatrix function described above. 
## cacheSolve function will also be able to get an inverse 
## from cache in some conditions.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("cached data will be obtained")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}


## Thanks for your attention!