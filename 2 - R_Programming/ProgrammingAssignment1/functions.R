add2 <- function(x, y) {
     x + y
}

above10 <- function(x) {
     use <- x > 10
     x[use]
}

above <- function(x, n = 10) {
     use <- x > n
     x[use]
}

columnmean <- function(y, removeNA = TRUE) {
     nc <- ncol(y)
     means <- numeric(nc)  ##empty vector of nc columns
     for(i in 1:nc) {
          means[i] <- mean(y[, i], na.rm = removeNA)
     }
     means
}

makeVector <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmean <- function(mean) m <<- mean
     getmean <- function() m
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean)
}

cachemean <- function(x, ...) {
     m <- x$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
}
