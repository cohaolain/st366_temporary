## sect 1
n <- d <- 1
pi.est <- n/d
eps <- 1e-6

while(abs(pi.est - pi) > eps) {
  if(pi.est < pi) n <- n + 1 else d <- d + 1
  pi.est <- n/d
}

c(n,d)
pi.est

## plot
n <- d <- 1
pi.est <- n/d
eps <- 1e-6

while(abs(pi.est[length(pi.est)] - pi) > eps) {
  if(pi.est[length(pi.est)] < pi) n <- n + 1 else d <- d + 1
  pi.est <- c(pi.est, n/d)
}

plot(pi.est, type = "l")
abline(h = pi, col = 2)

plot(pi.est[1:50], type="l")
abline(h = pi, col=2)
arrows(24,3.5,28,pi+0.05, code=2, length=0.05, angle=45, lwd=3)

## keep iterating
n <- d <- 1
pi.est <- n/d
fraction <- rbind(c(n,d))
i <- 1
max.iter <- 10000
index <- i

while(i < max.iter) {
  i <- i + 1
  if(pi.est[length(pi.est)] < pi) n <- n + 1 else d <- d + 1
  pi.est <- c(pi.est, n/d)
  dif <- abs(fraction[nrow(fraction),1]/fraction[nrow(fraction),2] - pi)
  newdif <- abs(n/d - pi)
  if(newdif < dif) {
    fraction <- rbind(fraction, c(n,d))
    cat("Iteration: ", i,
        "   Fraction: ", n, "/", d,
        "\n", sep = "")
    index <- c(index, i)
  }
}

plot(index, fraction[,1]/fraction[,2], xlim = c(1, max.iter))
plot(index[-c(1:6)], fraction[-c(1:6),1]/fraction[-c(1:6),2], xlim = c(1, 600))
abline(h = pi, lty = 2)

## sect 2
n <- 1000

x <- runif(n)
y <- runif(n)

xy <- cbind(x, y)
d <- apply(xy, 1, function(a) sqrt(sum(a^2)))

pi.est <- 4*sum(d < 1)/n
pi.est

plot(xy[d < 1,], xlim = c(0,1), ylim = c(0,1), pch = 21, bg = 2)
points(xy[d > 1,], pch = 21, bg = 4)

## wrapping into a function
sim.pi.est <- function(n) {
  x <- runif(n)
  y <- runif(n)
  xy <- cbind(x, y)
  d <- apply(xy, 1, function(a) sqrt(sum(a^2)))
  pi.est <- 4*sum(d < 1)/n
  return(list(pi.est = pi.est, xy = xy))
}

replicate(10, sim.pi.est(1000)$pi.est)
replicate(10, sim.pi.est(10000)$pi.est)
replicate(10, sim.pi.est(100000)$pi.est)

my.sim <- sim.pi.est(1e6)
print(my.sim$pi.est, digits = 7)
print(pi, digits = 7)

my.sim <- replicate(10000, sim.pi.est(100)$pi.est)
hist(my.sim, prob = TRUE)
lines(density(my.sim))
curve(dnorm(x, mean(my.sim), sd(my.sim)), add = TRUE, col = 2)
print(mean(my.sim), digits = 7)
print(pi, digits = 7)

# Buffon's Needle
buffon  <- function(n, # number of samples
                    l, # needle length
                    t  # line width
                    ) {
  if(l >= t) stop("The length of the needle must be shorter than the spacing of the lines")
  
  # Sample the location of the needle's centre.
  x     <- runif(n, min = 0, max = t  / 2)
  
  # Sample the angle of the needle wit respect to the lines
  theta <- runif(n, min = 0, max = pi / 2)
  
  # Does the needle cross a line?
  cross <- x <= l / 2  * sin(theta)
  
  # Estimate pi
  pi    <- (2 * l / (t * cumsum(cross))) * (1:n)
  pi    <- pi[is.finite(pi)]
  return(pi[length(pi)])
}
buffon(1000000, 1, 2)

# Obtain 1000 samples, each using 100000 needles
# and compute the median and a 95% credible interval
estimates <- replicate(1000, buffon(100000, 1, 2))
#hist(estimates)
#abline(v=pi, col="red", lty=2)
quantile(estimates, c(0.025, 0.5, 0.975))
#