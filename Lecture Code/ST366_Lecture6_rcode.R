sum.n <- function(n) sum(1:n)

sum(1:100)
sum(1:200)
sum(1:400)
sum(1:800)

sum.n(n=100) # same as sum.n(n = 100)
sum.n(n=200)
sum.n(n=400)
sum.n(n=800)

args(sum.n) # what are the arguments this function takes?

1:c(100, 200, 400, 800) # doesn't work
sum.n(n = c(100, 200, 400, 800)) # warning: only element "100" used

sapply(c(100,200,400,800), sum.n)
sapply(100*2^(0:3), sum.n)

sapply(100*2^(0:3), function(n) n*(n+1)/2)

u <- sapply(100*2^(0:3), sum.n) 
v <- sapply(100*2^(0:3), function(n) n*(n+1)/2)
u-v

compare.n <- function(n) {
  # calculating the sum using sum.n defined before
  a <- sum.n(n)
  # using the formula
  b <- n*(n+1)/2
  # returned object
  return(a-b)
}

# global variable
n <- 100
compare.n(n)

sapply(100*2^(0:3), compare.n)

compare.nr <- function(n, r = 1.08) {
  a <- sum(r^(0:n))
  b <- (r^(n+1)-1)/(r-1)
  return(a-b)
}

args(compare.nr)

compare.nr(20)
compare.nr(20, 1.08)
compare.nr(20, 1.06)

compare.nr(n = 20, r = 1)

compare.nr2 <- function(n, r = 1.08) {
  if(r == 1) {
    n+1
  } else     {
    a <- sum(r^(0:n))
    b <- (r^(n+1)-1)/(r-1)
    a-b
  }
}

compare.nr3 <- function(n, r = 1.08) {
  if(r == 1) return(n+1)
  a <- sum(r^(0:n))
  b <- (r^(n+1)-1)/(r-1)
  return(a-b)
}

r <- 1
n <- 20
sum(r^(0:n)) - (n+1)*r^n


f1 <- function(n) sum(1:n)
f2 <- function(n) n*(n+1)/2

f1(n) - f2(n)
#identical(f1(n), f2(n))
all.equal(f1(n), f2(n))

system.time(fans1 <- f1(n))
system.time(fans2 <- f2(n))

system.time(replicate(1000, f1(n)))
system.time(replicate(1000, f2(n)))

library(microbenchmark)
microbenchmark(f1(n), f2(n))

f <- function() {
  x <- 1
  g()
  return(x)
}

g <- function() x <- 2

f()
g()
x # yields an error
# check debug options

y <- 10
x <- 100

f <- function() {
  x <- 1
  return(x+y)
}

f()
x

#######################################################
f <- function() {
  x <- 1
  g()
}

g <- function() {
  x+5
}

x <- 100

g()
f()

## here, f is defined in the global environment
##       g is defined in the global environment: searches x in the global environment
#######################################################

#######################################################
f <- function() {
  h <- function() {
    x+5
  }
  x <- 1
  h()
}

h() # Error: could not find function "h"
f()

x <- 100
f()
# now comment line 97 and try again

## here, f is defined in the global environment
##       h is defined in f's local enviroment: searches for x in f's local environment
##       h doesn't exist in the global environment
#######################################################

x <- rexp(50, .2)
mean(x)
median(x)
mean(x, trim = .1)
mean(x, trim = .5)

x <- -3:6
# replace the smallest x by the next smallest
x[1] <- x[2]
x
# replace the biggest x by the next biggest
x[10] <- x[9]
x
mean(x)

x <- -3:6
# replace the smallest x by the next smallest
x[1:2] <- x[3]
x
# replace the biggest x by the next biggest
x[9:10] <- x[8]
x
mean(x)

n <- 20
k <- 3
set.seed(111); y <- sample(100, n)
x <- sort(y)
x
# replace the k smallest x values by the next smallest
x[1:k] <- x[k+1]
x
# replace the biggest k x values by the next biggest
x[(n-k+1):n] <- x[n-k]
x
mean(x)

wmean <- function(x, k) {
  # computes the winsorized mean
  n <- length(x)
  x <- sort(x)
  x[1:k] <- x[k+1]
  x[(n-k+1):n] <- x[n-k]
  return(mean(x))
}

wmean(x = y, k = 3)
wmean(y, 3)
wmean(3, y) # returns an error
wmean(k = 3, x = y)
wmean(k = 3, y)

wmean(y, 2)
wmean(y, 0) 
mean(y)

x <- c(-3,-2,0,2,7)
k <- 0
n <- length(x)
x[1:k] <- x[k+1]
x # no problems so far
x[(n-k+1):n] <- x[n-k]
x

wmean <- function(x, k) {
  # computes the winsorized mean
  if(k != 0) {
    n <- length(x)
    x <- sort(x)
    x[1:k] <- x[k+1]
    x[(n-k+1):n] <- x[n-k]
  }
  return(mean(x))
}

n <- 45
trim <- .25
trim*n
k <- floor(trim*n)
k

wmean <- function(x, trim) {
  # computes the winsorized mean
  n <- length(x)
  k <- floor(trim*n)
  k <- max(0, k) # bounds the minimum k to zero (trim=0)
  k <- min(k, floor((n-1)/2)) # bounds the maximum k to half of the values (trim=0.5)
  if(k != 0) {
    x <- sort(x)
    x[1:k] <- x[k+1]
    x[(n-k+1):n] <- x[n-k]
  }
  return(mean(x))
}

wmean(x = y, trim = .2)
wmean(x = y, trim = .5)
wmean(x = y, trim = .6)
wmean(x = y, trim = -1)

sapply(c(.2, .5, .6, -.1), wmean, x = y)
c(wmean(y, 0), mean(y), median(y))

## including an error message
wmean <- function(x, trim) {
  # check whether trim is inside acceptable range [0, 0.5]
  if(trim > 0.5 | trim < 0) stop("trim must be inside the [0, 0.5] interval")
  # computes the winsorized mean
  n <- length(x)
  k <- floor(trim*n)
  if(k != 0) {
    x <- sort(x)
    x[1:k] <- x[k+1]
    x[(n-k+1):n] <- x[n-k]
  }
  return(mean(x))
}

wmean(x = y, trim = .2)
wmean(x = y, trim = .5)
wmean(x = y, trim = .6)
wmean(x = y, trim = -1)
mean(y)

## including a warning
wmean <- function(x, trim) {
  # check whether trim is inside acceptable range [0, 0.5]
  if(trim > 0.5) {
    trim <- 0.5
    warning("trim was set as 0.5")
  }
  if(trim < 0) {
    trim <- 0
    warning("trim was set as 0")
  }
  # computes the winsorized mean
  n <- length(x)
  k <- floor(trim*n)
  if(k != 0) {
    x <- sort(x)
    x[1:k] <- x[k+1]
    x[(n-k+1):n] <- x[n-k]
  }
  return(mean(x))
}

wmean(x = y, trim = .2)
wmean(x = y, trim = .5)
wmean(x = y, trim = .6)
wmean(x = y, trim = -1)

## a simple experiment
y <- replicate(1000, {
  x <- rnorm(100)
  mean(x) - median(x)
  })

w <- replicate(1000, {
  x <- rexp(100)
  mean(x) - median(x)
  })

round(c(mean(y), mean(w)), 4)

y <- replicate(1000, {x <- rnorm(100); mean(x) - mean(x, trim = .2)})
w <- replicate(1000, {x <- rexp(100);  mean(x) - mean(x, trim = .2)})
round(c(mean(y), mean(w)), 4)

y <- replicate(1000, {x <- rnorm(100); mean(x) - wmean(x, trim = .2)})
w <- replicate(1000, {x <- rexp(100);  mean(x) - wmean(x, trim = .2)})
round(c(mean(y), mean(w)), 4)

diff_means <- function(x, func, ...) {
  mean(x) - func(x, ...)
}

y <- replicate(1000, diff_means(rnorm(1000), wmean, trim = 0.25))
w <- replicate(1000, diff_means(rexp(1000),  wmean, trim = 0.25))
round(c(mean(y), mean(w)), 4)

y <- replicate(1000, diff_means(rnorm(1000), mean, trim = 0.25))
w <- replicate(1000, diff_means(rexp(1000),  mean, trim = 0.25))
round(c(mean(y), mean(w)), 4)
