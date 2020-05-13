# random number generation

## Multiplicative congruential generator
m <- 7
a <- 3
seed <- 2

xi <- seed
n <- 7
x <- numeric(n)
for (i in 1:n) {
  xi <-  (a*xi) %% m ## note %% is the modulo operator
  x[i] <- xi
}
u <- x/m
x
u

MCG <- function(n, m, a, seed, b=0) {
  x <- rep(NA, n)
  xi <- seed
  for(i in 1:n) {
    x[i] <- xi <- (a*xi + b) %% m
  }
  return(x/m)
}

## longer
m <- 29241;  a <- 171; seed <- 3; 
xi <- seed; n <- 7
x <- numeric(n)
for (i in 1:n) {
  xi <-  (a*xi) %% m
  x[i] <- xi
}
x/m
MCG(7, 29241, 171, 3)

## using prime m
m <- 30269;  a <- 171; seed <-  27218; 
xi <- seed; n <- 500
x <- numeric(n)
for (i in 1:n) {
  xi <-  (a*xi) %% m
  x[i] <- xi
}
u <- x/m
u[1:20]
# MCG(500, 30269, 171, 27218)

## checking if this sequence would be a reasonable realisation 
## of uniform random variables
nbins <- 10
h <- hist(u, breaks = nbins)
ecount <- n/nbins # expected number in a bin
rect(0, 0, 1, ecount,
     border = NA, col = rgb(1, 0, 0, alpha = .2))

plot(u[-1], u[-n])

expected <- (1:n)/(n+1)
qqplot(u,expected)
abline(0,1,col="red",lwd=2)

p <- (1:6)/7
q <- p
curve(dunif, 0, 1, yaxs="i", xaxs="i", xaxt="n", ylim=c(0,1))
for (qi in q) lines(c(qi,qi), c(0, dunif(qi)))
axis(1,at=q, labels=round(q,1))

## conduct a chi-squared goodness-of-fit test
h$counts
(h$counts - ecount)^2/ecount
cstat <- sum((h$counts - ecount)^2/ecount)
pchisq(cstat, nbins-1,  lower.tail = FALSE)

# equivalent to
chisq.test(h$counts)

## discrete uniform
r <- runif(1000)
x <- ceiling(r*10)
table(x)
table(x)/1000 # compare observed and expected frequencies

hist(x, breaks = 0:10)
chisq.test(table(x))



## inverse transform theorem
inversion <- function(n, inv_func, ...) {
  u <- runif(n)
  return(inv_func(u, ...))
}

exp_inv <- function(u, lambda=1) {
  -(log(u))/lambda
}

lambda <- 2
n <- 1000
y <- inversion(n, exp_inv, lambda=lambda)
hist(y, prob = TRUE)
curve(dexp(x, lambda), add = TRUE, col = 2, lwd = 2)
lines(density(y), col = 4, lwd = 2)
mean(y)
## or y <- rexp(n, 2)

weibull_inv <- function(u, lambda=1, a=1) { # can be used in conjunction with the basic inversion function
  exp_inv(u, lambda)^(1/a)
} # see ?rweibull
inversion(10, weibull_inv, 2, 3)

gamma_int_inv <- function(n, k, lambda) { # a little more complicated, and only works for integer valued k
  inv_exp <- function(n, lambda) {
    -log(runif(n))/lambda
  }
  x <- matrix(inv_exp(n=n*k,lambda=lambda),ncol=k)
  rowSums(x)
} # see ?rgamma
gamma_int_inv(100, 10, 4)

unif_inv <- function(u, a, b) {
  a + (b - a) * u
}
inversion(10, unif_inv, a=1, b=3)
## runif(10, 1, 3)



## Monte Carlo integration
u <- runif(10000)
mean(u^4)

mcInt <- function(g, a, b, n = 10000) {
  u <- runif(n, a, b)
  return(mean(g(u))*(b-a))
}

mcInt(sin, 1, 2)
cos(1) - cos(2)
mcInt(sin, 1, 2, 10000000)

## Other random numbers
x <- rexp(100000)
g <- function(x) exp(-(x+1)^2)
mean(g(x)/dexp(x))
(1-pnorm(1,sd=1/sqrt(2)))*sqrt(pi)