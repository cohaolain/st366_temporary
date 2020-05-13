# Maximum likelihood estimation

## Normal distribution
mu <- 4
sigma <- 3
y <- rnorm(100, mu, sigma)

minusloglik <- function(parameters, y) {
  mu <- parameters[1]
  sigma <- parameters[2]
  return(- sum(dnorm(y, mu, sigma, log = TRUE)))
}

optim(c(4,3), minusloglik, y = y)$par
mean(y)              # closed form MLE for \mu
sd(y)                # closed form MLE for \sigma
sd(y)*sqrt(99/100)   # unbiased estimate of \sigma (not the MLE)

optim(c(4,0.0001), minusloglik, y = y)$par
optim(c(0.0001,3), minusloglik, y = y)$par

## Example 1: fitting a beta distribution
set.seed(123)
y <- rbeta(100, 1, .5)
hist(y, prob = TRUE)
lines(density(y))
curve(dbeta(x, 1, .5), col = 2, add = TRUE)

beta.loglik <- function(parameters, y) {
  p <- exp(parameters[1])
  q <- exp(parameters[2])
  llik <- dbeta(y, p, q, log = TRUE)
  return(-sum(llik))
}

inits <- c(0, 0) # initial values
fit <- optim(inits, beta.loglik, y = y, hessian = TRUE)
exp(fit$par)
sqrt(diag(solve(fit$hessian)))

beta.loglik2 <- function(parameters, y) {
  p <- parameters[1]
  q <- parameters[2]
  llik <- dbeta(y, p, q, log = TRUE)
  return(-sum(llik))
}
inits <- c(1,1)
fit2 <- optim(inits, beta.loglik2, y = y, hessian = TRUE,
              method = "L-BFGS-B", lower = c(0,0), upper = c(Inf,Inf))
fit2$par; exp(fit$par)
exp(fit$par) * sqrt(diag(solve(fit$hessian))); sqrt(diag(solve(fit2$hessian)))

hist(y, prob = TRUE)
lines(density(y), lty = 2)
curve(dbeta(x, 1, .5), col = 4, lwd = 2, add = TRUE)
curve(dbeta(x, fit2$par[1], fit2$par[2]), col = 2, lwd = 2,
      add = TRUE, xlim = c(.01, .99))

## Example 2: AR(1) model
fAR1 <- function(n, b0, b1, sigma2, y1=0) {
  y <- c(y1, rep(0, n-1))
  for(i in 2:n) y[i] <- b0 + b1*y[i-1] + rnorm(1, 0, sqrt(sigma2))
  return(y)
}

set.seed(24)
y <- fAR1(100, 2, .7, 1.5)
#y <- fAR1(100, 2, .7, 1.5, 2)
plot.ts(y)

ar1lik <- function(parameters, y) {
  n <- length(y)
  beta0 <- parameters[1]
  beta1 <- parameters[2]
  sigma2 <- parameters[3]
  lik.y1 <- dnorm(y[1], beta0/(1+beta1), sqrt(sigma2/(1+beta1^2)), log=TRUE)
  lik.yt <- sum(dnorm(y[2:n], beta0+beta1*y[1:(n-1)], sqrt(sigma2), log=TRUE))
  full.lik <- lik.y1 + lik.yt
  return(-full.lik)
}

fit1 <- optim(c(0,.5,1), ar1lik, y = y, hessian = TRUE,
              method = "L-BFGS-B", lower = c(-Inf,-1,0), upper = c(Inf,1,Inf))
ep1 <- sqrt(diag(solve(fit1$hessian)))
round(data.frame(beta0=c(2, fit1$par[1], ep1[1]),
                 beta1=c(0.7, fit1$par[2], ep1[2]),
                 sigma2=c(1.5, fit1$par[3], ep1[3]),
                 row.names=c("True value", "Estimate", "Std error")), 4)

ar1lik2 <- function(parameters, y) {
  parameters[3] <- fit1$par[3]
  ar1lik(parameters, y)
}

devFun <- function(parameters, est, llFUN, ...) {
  llFUN <- match.fun(llFUN)
  return(2 * (llFUN(parameters, ...) - llFUN(est, ...)))
}

devSurf <- Vectorize(function(a,b, ...) devFun(c(a,b), ...))

b0 <- seq(.6, 3, length=100)
b1 <- seq(.5, .9, length=100)
lik <- outer(b0, b1, devSurf, llFUN = ar1lik2, y = y, est = fit1$par[1:2])
LEVELS <- c(0.99,0.95,0.9,0.7,0.5,0.3,0.1,0.05)
contour(b0, b1, lik, levels=qchisq(LEVELS,df=2), labels=LEVELS,
        xlab=expression(beta[0]), ylab=expression(beta[1]))

require(lattice)
my.palette <- colorRampPalette(c("darkblue", "cyan"))
wireframe(lik, drape = TRUE, colorkey = TRUE, col.regions = my.palette(100),
          xlab = expression(beta[0]), ylab = expression(beta[1]),
          zlab = "Deviance", screen = list(z=-30, x=-70, y=0), col = NA)
#