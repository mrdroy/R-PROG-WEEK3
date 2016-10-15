# R-PROG-WEEK3
## Caching the Invesrse of a Matrix:
## Matrix inverse is useually a costly computation and there may be some 
## benefit to caching the inverse of a matirx rather than compute it repeatedly. 
## Below are functions that create a special object which
## stores a matrix and cache inverse of it. 

## This function creates a speacial "matrix" object and that can cache its inverse.

makeCacheMatrix <- function(a = matrix()){
         inv <- Null
         set <- function(b) {
                 a <<- b 
                 inv <<- NULL
         }
         get <- function() a
         setInverse <- function(inverse) inv<<- inverse 
         getInverse <- function() inv
         list(set=set,
              get=get)
              setInverse = setInverse
              getInverse = getInverse
}



## This function computes the inverse of special "matrix" created by
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retireve the inverse from the cache. 

cacheSolve <- function(a,.....){
          ## Return a matrix that is inverse of 'a'
          inv <- a$getInverse()
          if(!is.null(inv)){
                 massage("getting cached data")
                 return(inv)
          }
          mat <- a$get()
          inv <- solve(mat,....)
          a$setInverse(inv) 
          inv
}          
