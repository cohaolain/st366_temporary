### Name: First Last
### Number: 12345678

## Q1
# a
library(MoEClust)
data(ais, package="MoEClust")
?ais
plot(RCC ~ BMI, data = ais, col=ais$sex)
#plot(RCC ~ BMI, data = ais, pch=21, bg=c("magenta", "yellow")[as.numeric(ais$sex)])

# b
mean(ais$RCC)
# 4.718614

# c
mean(ais$RCC[ais$sex == "male"])
# 5.026569

# d
mean(ais$RCC[ais$sport == "Tennis"])
# 4.790909

# e
mean(ais$RCC[ais$sex == "male" & ais$sport == "Tennis"])
# 5.26

# f
plot(RCC ~ sex, data = ais)

## Q2
# a
dgumbel <- function(x, beta, mu) {
  z     <- (x - mu)/beta
  return(1/beta * exp(-z - exp(-z)))
  
}
dgumbel(x = c(-1,0,1), beta = 1/2, mu = -1)

# b
dgumbel2 <- function(x, beta=1, mu=0) {
  z      <- (x - mu)/beta
  return(1/beta * exp(-z - exp(-z)))
  
}

# c
dgumbel3 <- function(x, beta=1, mu=0) {
  if(beta <= 0) {
    stop("beta must be strictly positive")
  } else {
    dgumbel2(x, beta, mu)
  }
}

# d
x <- seq(-5, 20, by = 0.01)
dg1 <- dgumbel3(x = x, beta = 4, mu = 2)
dg2 <- dgumbel3(x = x, beta = 2, mu = 1)
dg3 <- dgumbel3(x = x)
plot(x, dg1, type = "l", ylim=c(0,0.4), col="red", ylab="Density")
lines(x, dg2, col = 3)
lines(x, dg3, col = 4)
# matplot(cbind(dg1, dg2, dg3), lty=1, col=2:4, type="l", ylim=c(0,0.2), ylab="Density", xlab="x")

## Q3
# a
N <- rep(NA, 20)
i <- 1
r <- 2.5
K <- 25
N[1] <- 6
while(i < 20) {
  N[i+1] <- N[i] * exp(r * (1 - N[i]/K))
  i <- i + 1
}
N

# b
Ricker <- function(N1, r, K, time) {
  N <- rep(NA, time)
  i <- 1
  N[1] <- N1
  while(i < time) {
    N[i+1] <- N[i] * exp(r * (1 - N[i]/K))
    i <- i + 1
  }
  return(N)
}

# c
plot(Ricker(N1 = 6, r = 2.5, K = 25, time = 20), type = "l", lty=1, lwd=1)
plot(Ricker(N1 = 6, r = 3,   K = 25, time = 20), type = "l", lty=2, lwd=2)
plot(Ricker(N1 = 6, r = 3.5, K = 25, time = 20), type = "l", lty=3, lwd=3)

# d
N <- 6
r <- 2.5
K <- 25
for(i in 1:19) N[i+1] <- c(N, N[i] * exp(r * (1 - N[i]/K)))
N
# The warnings indicate that we are trying to replace more than one
# element into a single element slot in a vector
# i.e. when we do N[i+1] <- c(N, ...)
# we are attempting to assign a vector with more than one element
# to position i+1 within vector N, and hence R will assign only
# the first element of c(N, ...) to position i+1 of N, which is
# N[1] = 6 That is why the resulting vector consists only of 6s.
#