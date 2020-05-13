### example 1
nyears <- 0
balance <- 100
target <- 200
rate <- 0.03

while(balance < target) {
  balance <- balance + rate*balance
  nyears <- nyears + 1
}
nyears

nyears <- 1
balance <- 100
target <- 200
rate <- 0.03
while(balance[nyears] < target) {
  balance <- c(balance, balance[nyears] + rate*balance[nyears])
  nyears <- nyears + 1
}
nyears - 1
plot(0:(nyears - 1), balance, type = "o")
abline(h = target, lty = 2)

### example 2
fibs <- c(1,1)
i <- 2 # number of fibs so far
newfib <- fibs[2] + fibs[1]

while (newfib < 300) {
  i <- i+1
  fibs[i] <- newfib
  newfib <- fibs[i] + fibs[i-1]
}
fibs
i

fibs <- numeric(100) # allocate space for the fibs
fibs[1:2] <- 1
i <- 2 # number of fibs so far
newfib <- fibs[2] + fibs[1]

while(newfib < 300) {
  i <- i+1
  fibs[i] <- newfib
  newfib <- fibs[i] + fibs[i-1]
}
fibs <- fibs[1:i]
plot(fibs)

# CAUTION
# be very careful you don't get stuck in an "infinite" while loop
# the code below will cause R to crash or hang

fibs <- numeric(100) # allocate space for the fibs
fibs[1:2] <- 1
i <- 2 # number of fibs so far
newfib <- fibs[2] + fibs[1]

while(newfib > 0) { # ALWAYS TRUE
  i <- i+1
  fibs[i] <- newfib
  newfib <- fibs[i] + fibs[i-1]
}
fibs <- fibs[1:i]
plot(fibs)

### example 3
n <- 15
fib <- numeric(n)
fib[1:2] <- 1
for(i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
fn <- fib[-1]
fn1 <- fib[-n]
plot(fn/fn1)
abline(h = (1+sqrt(5))/2, col="red")

fib1 <- 1
fib2 <- 1
n <- 2
golden <- (1+sqrt(5))/2
eps <- 1e-100
while(abs(fib2/fib1 - golden) >= eps) {
  fibnew <- fib1 + fib2
  fib1 <- fib2
  fib2 <- fibnew
  n <- n+1
  print(fib2/fib1)
}
n

### example 4
step <- function() rbinom(1,1,.5) * 2 - 1

position <- 0
nsteps <-  0
target <- 5
while(abs(position) < target) {
  position <- position + step()
  nsteps <- nsteps+1
}
nsteps

nsteps <- function(target) {
  position <- 0
  n <- 0
  while (abs(position) < target) {
    position <- position + step()
    n <- n+1
  }
  return(n)
}

replicate(6, nsteps(5))

par(mfrow=c(1,2))
steps <- replicate(10000, nsteps(5))
boxplot(steps)
hist(steps)

mean(steps)
median(steps)

step <- function() rnorm(1)

par(mfrow=c(1,2))
steps <- replicate(200, nsteps(5))
boxplot(steps)
hist(steps)
