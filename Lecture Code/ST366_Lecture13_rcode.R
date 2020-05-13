# density estimation
set.seed(123)
x <- rexp(50,4)

x[1:10]
lamhat <- 1/mean(x)

hist(x,freq=FALSE)
#x.grid <- seq(0, 2, length = 100)
#fhat <- lamhat*exp(-lamhat*x.grid)
#lines(x.grid, fhat, col = 4, lwd = 3)
curve(dexp(x,lamhat), add = TRUE)
hist(x,freq=FALSE,ylim=c(0,5))
curve(dexp(x,lamhat), add = TRUE)
#curve(dexp(x,4), add = TRUE, col = 2)

## ?curve
# normal example
#y <- rnorm(1000, 2, 4)
#hist(y, prob = TRUE)
#mu.hat <- mean(y)
#sig.hat <- sd(y)
#curve(dnorm(x, mu.hat, sig.hat), add = TRUE, col = 3, lwd = 2)

h <- hist(x,plot=FALSE)
h$density
h$mids
# abline(v = h$mids, col = 2, lty = 2)
dexp(h$mids, lamhat)

#hist(x,freq=FALSE,ylim=c(0,5))
#curve(dexp(x,lamhat),add=TRUE)
#points(h$mids, dexp(h$mids, lamhat), type = "h", col = 2, lwd = 3)

p <- (1:6)/7
q <- qexp(p)
curve(dexp, 0,6, yaxs="i", xaxs="i", xaxt="n") # "pretty" labels, look at ?par
#curve(dexp, 0,6, xaxt="n")
for(qi in q) lines(c(qi,qi), c(0, dexp(qi)))
axis(1, at=q, labels=round(q,1), cex.axis=1, las=2)

sortx <- sort(x)
q <- qexp((1:50)/51,lamhat)
rbind(sortx,q)[,1:6]

par(mfrow=c(1,2))
plot(sortx,q)
qqplot(x,q)
dev.off()

## another example
require(MASS)
geyser[1:5,]
wait <- geyser[,1]

par(mfrow=c(1,2))

lamhat <- 1/mean(wait)
hist(wait,freq=FALSE)
curve(dexp(x,lamhat),add=TRUE)

q <- qexp((1:299)/300,lamhat)
qqplot(wait,q)
dev.off()

mu <- mean(wait)
sigma <- sd(wait)
hist(wait, freq=FALSE)
curve(dnorm(x, mu, sigma), add=TRUE) 
# note that x is supplied to the dnorm function inside the curve function, rather than wait
# this is because the first argument to dnorm always has the name "x". 
# curve(add=TRUE) impicitly knows that the x argument relates to the wait data already on the existing plot

# in contrast
# curve(dnorm, add=TRUE, col="red")
# would add the standard normal density without supplying anything to dnorm
# in other words, the default dnorm arguments are all used
# in this instance, the standard normal is obviously awful because "wait" is centered around ~72, not zero.

## histogram as a density estimator
h <- hist(wait,freq=FALSE)
h$breaks
which.min(h$breaks <= 61) - 1
i <- sum(h$breaks <= 61)
i
h$density[i]

h1 <- hist(wait,freq=FALSE,breaks="Scott")
i <- which.min(h1$breaks <= 61) - 1
h1$density[i]

## frequency polygons
h <- hist(wait,freq=FALSE)
vx <- h$mids
vy <- h$density
gapx <- diff(vx)[1]
vx <- c(vx[1] - gapx, vx,vx[length(vx)] + gapx)
vy <- c(0,vy,0)

par(mfrow=c(1,2))
plot(vx,vy,type="l", main="Frequency polygon",cex.main=.8)

plot(vx,vy,type="l", main="Frequency polygon with histogram",cex.main=.8)
hist(wait,freq=FALSE,add=TRUE)
dev.off()

fp <- approxfun(vx,vy)
fp(61) # the density estimate at x=61
integrate(fp,lower=min(vx),upper=max(vx))

## average shifted histogram
require(MASS)
wait <- geyser[,1]
range(wait)
b1 <- seq(36, 108, by = 9) # sequence of breaks for a histogram
b2 <- b1 + 3 # other breaks
b3 <- b1 + 6 # other breaks
h1 <- hist(wait, breaks = b1, freq = FALSE, xlim = c(36,114), col = "#FF000044")
h2 <- hist(wait, breaks = b2, freq = FALSE, add = TRUE, col = "#00FF0044")
h3 <- hist(wait, breaks = b3, freq = FALSE, add = TRUE, col = "#0022FF44")

histheight <- function(x, h) { # h is the histogram object
  b <- h$breaks
  if(x < min(b) || x >= max(b)) {
    return(0)
  } else {
    i <- sum(h$breaks <= x)
    return(h$density[i])
  }
}

histheight(61, h1)
histheight(61, h2)
histheight(61, h3)

mean(c(histheight(61,h1),histheight(61,h2),histheight(61,h3)))

ax <- sort(c(b1,b2,b3)) 
ax[1:6]
# what is the bin width?
ax <- ax+1.5 # midpoints
ay1 <- sapply(ax, histheight, h1)
ay2 <- sapply(ax, histheight, h2)
ay3 <- sapply(ax, histheight, h3)
cbind(ay1,ay2,ay3)[1:5,]
ay <- apply(cbind(ay1,ay2,ay3),1,mean)
ay[1:6]
lines(ax, ay, lwd = 3)

par(mfrow=c(1,2))
plot(ax,ay,type="l", main="ASH histogram",cex.main=.8)

plot(ax,ay,type="l", main="ASH histogram",cex.main=.8)
hist(wait,breaks=b1,freq=FALSE,add=TRUE)
dev.off()

ash <- approxfun(ax,ay)
ash(61) # the density estimate at x=61
integrate(ash,lower=min(ax),upper=max(ax))

require(ash)
f <- ash1(bin1(wait,nbin=30))
lines(f, lwd = 3, col = 2)

hist(wait, prob = TRUE, breaks = "Scott")
lines(ax, ay, lwd = 3)
lines(f, lwd = 3, col = 2)

plot(f, type="l" ,main="ASH from package")
fhat <- approxfun(f$x,f$y)
fhat(61)
integrate(fhat,lower=min(f$x),upper=max(f$x))

plot(f, type="l" ,ylim=c(0,.035))
lines(ax,ay,type="l",col="red")

## kernel density estimation
par(mfrow=c(2,2))
wr <- function(x) {
  ifelse(abs(x) < 1, 1/2,0)
}

we <- function(x) {
  ifelse(abs(x) < 1, (3/4)*(1-x^2),0)
}

wbw <- function(x) {
  ifelse(abs(x) < 1, (15/16)*(1-x^2)^2,0)
}

curve(wr,-1.5,1.5, main="rectangular")
curve(we,-1.5,1.5, main="epanechnikov")
curve(wbw,-1.5,1.5, main="biweight")
curve(dnorm,-3,3, main="gaussian")
dev.off()

require(MASS)
wait <- geyser[,1]
density(wait)
d <- density(wait)
plot(d)

d$x[1:4]
d$y[1:4]

fhat <- approxfun(d$x,d$y)
fhat(61)

par(mfrow=c(2,2))
plot(density(wait,bw=1), main = "Bandwidth 1")
plot(density(wait,bw=2), main = "Bandwidth 2")
plot(density(wait,bw=8), main = "Bandwidth 8")
plot(density(wait,bw=16), main = "Bandwidth 16")
dev.off()

density(wait, kernel="rectangular")
plot(density(wait, kernel="rectangular"))

par(mfrow=c(2,2))
plot(density(wait,bw=1, kernel="rectangular"), main = "Bandwidth 1")
plot(density(wait,bw=2, kernel="rectangular"), main = "Bandwidth 2")
plot(density(wait,bw=8, kernel="rectangular"), main = "Bandwidth 8")
plot(density(wait,bw=16, kernel="rectangular"), main = "Bandwidth 16")
dev.off()

## Side note
# use curve for a theoretical parameterised density!
# use lines for an estimated density!
mu <- mean(wait)
sigma <- sd(wait)
hist(wait, freq=FALSE)
curve(dnorm(x, mu, sigma), add=TRUE) 
lines(density(wait), col="red")

## bivariate normal density
dnorm2 <- function(x,y,mx=0,my=0,sx=1,sy=1,rho=0){
  x1 <- (x-mx)/sx
  y1 <- (y-my)/sy
  z <- x1^2 - 2*rho*x1*y1+ y1^2
  a <- exp(-z/(2*(1-rho^2)))
  b <- 2*pi*sx*sy*sqrt(1-rho^2)
  return(a/b)	
}

dnorm2(1,1)
dnorm2(1,1,rho=.5)

x <- 1:4
y <- 8:10
outer(x,y,"*")

x <- seq(-3,3,length=50)
y <- seq(-3,3,length=50)
z <- outer(x,y,dnorm2)
z[1:4,1:4]

par(mfrow=c(2,2))
persp(x,y,z)

persp(x,y,z,theta=45,phi=30) # change viewing angle
contour(x,y,z,asp=1)

greys <- rev(gray(seq(0,1,.05)))
image(x,y,z,col=greys,asp=1)

x <- seq(-3,3,length=50)
y <- seq(-3,3,length=50)
z <- outer(x,y,dnorm2,rho=.5)
z[1:4,1:4]

par(mfrow=c(2,2))

persp(x,y,z)

persp(x,y,z,theta=45,phi=30) # change viewing angle
contour(x,y,z,asp=1)
image(x,y,z,col=greys,asp=1)
dev.off()

## bivariate data
require(MASS)
plot(geyser)

gx <- 4:11 * 10
gy <- 1:6 - .5
abline(v = gx, col = "blue", lty = 2)
abline(h = gy, col = "blue", lty = 2)

waitf <- cut(geyser[,1], breaks = gx)
durationf <- cut(geyser[,2], breaks = gy)
tab <- table(waitf, durationf)
tab
#table(durationf)
#table(waitf)

plot(geyser)

#gray(0)
#gray(1)
greys <- rev(gray(seq(0, 1, .05)))
greys1 <- col2rgb(greys)/255
greys1 <- rgb(t(greys1), alpha = .3) # transparent version of greys

image(gx, gy, tab, col = greys1, add = TRUE)

par(mfrow=c(1,2))

require(ash)
b <- bin2(as.matrix(geyser), nbin = c(50,50))
h <- ash2(b)

image(h, col = greys)
contour(h)
dev.off()

filled.contour(h, col = greys)

par(mfrow = c(1,2))
f <- kde2d(geyser$waiting, geyser$duration)
image(f, col = greys)
contour(f)
dev.off()

image(f, col = rev(heat.colors(21)))
contour(f, add = TRUE)
points(geyser, cex = .8, pch = 21, bg = "lightblue")

my.palette <- colorRampPalette(c("white", "lightblue", "#15e7a3"))
image(f, col = my.palette(100), bty = "l", las = 1,
      xlab = "Waiting time (min)", ylab = "Duration (min)")
contour(f, add = TRUE, lty = 2)
points(geyser, cex = .8, pch = 21, bg = "yellow", col = "#ab0101")

### using ggplot2
require(ggplot2)
ggplot(geyser, aes(x = waiting, y = duration)) + theme_bw() +
  geom_density_2d() +
  geom_point()

ggplot(geyser, aes(x = waiting, y = duration)) + theme_bw() +
  geom_hex()

#### example using double for loop vs. outer
A <- matrix(0, ncol = 3, nrow = 4)
for(i in 1:4) {
  for(j in 8:10) {
    A[i,j-7] <- i * j
  }}
A

outer(1:4, 8:10, "*")
