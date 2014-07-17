## Functions used to create a matrix object which can cache its inverse

## This function creates a special "matrix" object that can cache its inverse.
## It contains functions to:
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse of a matrix
## 4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
     s<-NULL;
     # set matrix value and set solve to null
     set<-function(y) {
          x<<-y;
          s<<-NULL;
     }
     # returns the matrix value
     get<-function() x;
     # caches the value of solve
     setsolve<-function(solve)s<<-solve;
     # returns the cached value of solve
     getsolve<-function() s;
     
     list(set = set, get = get, setsolve = setsolve, 
          getsolve = getsolve);
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
     ## if the inverse is already cached, return it
     s<-x$getsolve();
     if(!is.null(s)) {
          message("getting cached data");
          return(s);
     }
     ##create inverse and cache it
     data<-x$get();
     s<-solve(data,...);
     x$setsolve(s);
     
     ## Return a matrix that is the inverse of 'x'     
     s
}
