### Name - Keefe Murphy
### Student No. - 12345678

## Q1
# a

# install.packages("MASS")
library(MASS)
?cats
data(cats)
plot(Bwt ~ Hwt, data=cats)
#plot(cats$Bwt ~ cats$Hwt)
#plot(cats$Hwt, cats$Bwt)

# b
mHwt <- mean(cats$Hwt)
mHwt # 10.63056

mean(cats$Bwt[cats$Hwt < mHwt])
# 2.442169
mean(cats$Bwt[cats$Hwt > mHwt])
# 3.106557

# c
cBwt <- cut(cats$Bwt, breaks=3)
boxplot(cats$Hwt ~ cBwt)

## Q2
# a
infec <- rpois(n=30, lambda=12)

# b
(cumul_infec <- cumsum(infec))

# c
plot(cumul_infec, type="l")
lines(cumsum(rpois(30, 12)), col=2)
lines(cumsum(rpois(30, 12)), col=4)

## Q3
# a
R <- 1.4
K <- 10
time <- 30

n <- numeric(time)
n[1] <- 3

for(i in 2:time) {
  n[i] <- (K * R * n[i-1]) / (K + n[i-1] * (R - 1))
}
n

# b
bhmodel <- function(R, K, n1, time) {
  n <- numeric(time)
  n[1] <- n1
  
  for(i in 2:time) {
    n[i] <- (K * R * n[i-1]) / (K + n[i-1] * (R - 1))
  }
  return(n)
}

all.equal(bhmodel(R=1.4, K=10, n1=3, time=30), n)

# c
plot(bhmodel(R=1.4, K=10, n1=3, time=30), type="l")
lines(bhmodel(R=1.6, K=10, n1=3, time=30), col=2)
lines(bhmodel(R=1.8, K=10, n1=3, time=30), col=3)
lines(bhmodel(R=2, K=10, n1=3, time=30), col=4)

  # alternative solution
  scenarios <- sapply(c(1.4, 1.6, 1.8, 2), bhmodel, 
                      K=10, n1=3, time=30)
  matplot(scenarios, type="l", lty=1)

# d
  solution <- c(n[1], (K * n[1]) / (n[1] + (K - n[1]) * R^(-(1:(time - 1)))))
  all.equal(bhmodel(R=1.4, K=10, n1=3, time=30), solution)

# e  
  paramecium <- c(2, 3, 29, 92, 173, 210, 240, 240, 240, 240, 219, 255, 252, 270, 240, 249)

  y <- paramecium[-length(paramecium)]/paramecium[-1]
  x <- paramecium[-length(paramecium)]
  
  fit <- lm(y ~ x)
  betas <- coef(fit)
  beta0 <- betas[1]
  beta1 <- betas[2]
  
  R.hat <- 1/beta0
  K.hat <- (R.hat - 1) / (R.hat * beta1)
  
  plot(paramecium)
  fitted <- bhmodel(R = R.hat, K = K.hat, 
                    n1=paramecium[1],
                    time=length(paramecium))
  lines(fitted, col=4, lwd=2)  
###  