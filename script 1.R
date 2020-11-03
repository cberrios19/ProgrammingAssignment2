
makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() {x}
  setinversa <- function(inversacal){inversa<<- inversacal}
  getinversa <- function() {inversa}
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}
##
cachesolve <- function(x, ...) {
  inversa <- x$getinversa()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinversa(inversa)
  inversa
}