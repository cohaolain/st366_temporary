#### if statements
y <- -1

if(y > 0) print("non negative") else print("negative")

if(y > 0) {
  print("non negative")
} else print("negative")

if(y > 0) print("non negative") else {
  print("negative")
}

if(y > 0) print("non negative")
else print("negative") # gives an error

if(y > 0) {
  print("non negative")
}
else print("negative")

### Example 1
x <- 12
if (x > 0) {
  print("non negative")
} else {
  print("negative")
}

x <- rnorm(5)
ifelse(x > 0, "positive", "negative")
ifelse(TRUE, c(1, 2), c(3, 4))
ifelse(c(TRUE, FALSE), c(1, 2), c(3, 4))

### Example 2
qroots <- function(a,b,c) {
  d <- sqrt(b ^ 2 - 4 * a * c)
  c((-b - d) / (2 * a), (-b + d) / (2 * a))
}

qroots(1,-3,2)
qroots(1,1,1) 

qroots <- function(a,b,c) {
  d <- b^2 - 4*a*c
  if(d >= 0) {
    d1 <- sqrt(d)
    roots <-  c( (-b -d1)/(2*a), (-b +d1)/(2*a))
    } else {
      roots <- c()
    }
  roots
}

qroots(1,-3,2)
qroots(1,1,1) 
qroots(1,4,4)

qroots <- function(a,b,c) {
  d <- b^2 - 4*a*c
  if(d > 0) {
    d1 <- sqrt(d)
    roots <-  c( (-b -d1)/(2*a), (-b +d1)/(2*a))
    return(roots)
  }
  if(d==0) {
    r <-  -b/(2*a)
    roots <- c(r,r)
    return(roots)
  }
  roots <- c()
  return(roots)
}

qroots(1,-3,2)
qroots(1,4,4) 
qroots(1,1,1)

## what if a = 0?
qroots(0,1,-1)

### Example 3
marks <- c(55,87,56,90,23,44)
# First we need to find the min, 
# then remove it from marks
w <- which.min(marks) # Gives the position of the min
w
mean(marks[-w])

# using sort()
marks.s <- sort(marks)
mean(marks.s[-1])

# which one is faster?
x <- rnorm(6)
system.time(replicate(100000, mean(sort(x)[-1])))
system.time(replicate(100000, mean(x[-which.min(x)])))

# better to use
microbenchmark::microbenchmark(mean(sort(x)[-1]),
                               mean(x[-which.min(x)]))

marks[2] <- NA
w <- which.min(marks)
w
mean(marks[-w]) # This is obviously not the intended result

CAmark <- function(marks) {
  missing <- is.na(marks)
  if (any(missing)) {
    sum(marks, na.rm=TRUE)/(length(marks)-1)
  } 
  else {
    w <- which.min(marks)
    mean(marks[-w]) 
  }
}  

CAmark(c(55,87,56,90,23,44))
CAmark(c(NA,87,56,90,23,44))
CAmark(c(NA,87,56,90,NA,44))

### another way
CAmark2 <- function(marks) {
  if(any(is.na(marks))) marks[is.na(marks)] <- 0
  w <- which.min(marks)
  mean(marks[-w])
}

CAmark2(c(55,87,56,90,23,44))
CAmark2(c(NA,87,56,90,23,44))
CAmark2(c(NA,87,56,90,NA,44))

#### for statements
### example 1
x <- rnorm(5)
sum(x)
sumx <- 0
for(xi in x) sumx <- sumx+xi
sumx

x <- rnorm(5)
sum(x[x>0])
sumx <- 0
for (xi in x) {
  if (xi > 0) sumx <- sumx+ xi
}
sumx
xi # the last value in the vector

### example 2 (class)
fib <- numeric(12)
fib[1:2] <- 1
for(i in 3:12) fib[i] <- fib[i-1] + fib[i-2]
fib

fib2 <- c(1,1)
for(i in 3:12) fib2 <- c(fib2, fib2[i-2] + fib2[i-1])
fib2

system.time(replicate(300000, {
  fib <- rep(NA, 12)
  fib[1:2] <- 1
  for(i in 3:12) fib[i] <- fib[i-1] + fib[i-2]
}))

system.time(replicate(300000, {
  fib2 <- c(1,1)
  for(i in 3:12) fib2 <- c(fib2, fib2[i-2] + fib2[i-1])
}))

### example 3
rbinom(1,1,.5)
?rbinom
rbinom(1,1,.5) * 2 - 1

sample(c(-1,1), 1)
sum(replicate(1000, sample(c(-1,1), 1)))
sum(replicate(1000, rbinom(1,1,.5) * 2 - 1))

step <- function() rbinom(1,1,.5) * 2 -1

step()
step()

walk <- numeric(10)
walk[1] <- 0
for (i in 2:10) walk[i] <- walk[i-1] + step()

rwalk <- function(n) {
  walk <- numeric(n)
  walk[1] <- 0
  for(i in 2:n) walk[i] <- walk[i-1]+ step()
  walk
}

n <- 100
w <- rwalk(n)
w
plot(1:n, rwalk(n), col = 1,
     type = "l", ylim = c(-20,20))
lines(1:n, rwalk(n), col = 2)
lines(1:n, rwalk(n), col = 3)
lines(1:n, rwalk(n), col = 4)

set.seed(2020)
rwalk(n)

## Example 4
paramecium <- c(2,3,29,92,173,210,240,240,
                240,240,219,255,252,270,240,249)
plot(paramecium)

y <- log(paramecium[-1]/paramecium[-length(paramecium)])
#y <- log(paramecium[-1]) - log(paramecium[-length(paramecium)]) 
# more numerical stability by working entirely on the log scale, at the expense of speed
x <- paramecium[-length(paramecium)]
est <- lm(y ~ x)
betas <- coef(est)
r.hat <- betas[1]
K.hat <- -r.hat/betas[2]
r.hat
K.hat

n.hat <- numeric(length(paramecium))
n.hat[1] <- paramecium[1]
for(i in 2:length(paramecium)) {
  n.hat[i] <- n.hat[i-1]*exp(r.hat*(1-n.hat[i-1]/K.hat))
}
lines(n.hat, col = "red")

####SPEARMAN'S RANK CORRELATION
x <- c(4.1,6.2,1.3,2.1)
y <- c(11,16,19,2.1)

plot(x, y, xlab="x", ylab="y", main="",
     cex.lab=1.6,cex.axis=1.75,
     mfrow=par(mar=c(5, 5, 1, 1)), cex=2)

Rx <- rank(x)
Ry <- rank(y)

plot(Rx, Ry, xlab="Rx", ylab="Ry", main="",
     cex.lab=1.6,cex.axis=2,
     mfrow=par(mar=c(5, 5, 1,1)), cex=2)

library(gtools)

perms1 <- permutations(4, 4, c(1,2,3,4))

estimates <- numeric(24)
for (i in 1:24) {
  estimates[i] <- cor(Rx, perms1[i,])
}

## or
## estimates2 <- apply(perms1, 1, function(x) cor(Rx, x))

output <- cbind(perms1, estimates)
output

r.obs <- cor(Rx, Ry)
sum(abs(output[,5]) >= abs(r.obs))/24

!(abs(output[,5]) >= abs(r.obs))