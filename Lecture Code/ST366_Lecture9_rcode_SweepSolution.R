x <- seq(.1,.5,by=.1)
x
lx <- sapply(x, log)

lx2 <- numeric(5)
for(i in 1:5) {
  lx2[i] <- log(x[i]) 
}
lx2

log(x)


### sapply - example 1
sumn <- function(n) sum(1:n)
sumn(100)
sumn(200)
sumn(c(100,200,400,800))

sapply(c(100,200,400,800), sumn)

### sapply - example 2
y <- rnorm(40)
mean(y, trim=.1)

trims <-  seq(.0, .5, by=.1)
trmeans <- numeric(length(trims))
for (i in 1:length(trims)) {
  trmeans[i] <- mean(y, trims[i])
}
trmeans

sapply(trims, function(tr) mean(y, tr))

my.function <- function(observations, tr) {
  mean(x = observations, trim = tr)
}

sapply(trims, my.function, observations = y)

sapply(trims, mean, x=y)

### apply - example 1
mtcars54 <- mtcars[1:5,1:4]
apply(mtcars54, 2, mean) # calculates the mean of each variable
apply(mtcars54, 2, mean, .1) # calculates a trimmed mean of each variable
apply(mtcars54, 1, median) # calculates the median of each car model

colMeans(mtcars54)
rowMeans(mtcars54)

### apply - example 2
my.function <- function(x, y) sum(x) * y
apply(mtcars54, 2, my.function) # returns an error
apply(mtcars54, 2, my.function, y = 2)

apply(mtcars54, 2, function(a, b) mean(a) + b, b = 100)
apply(mtcars54, 2, function(a, b) a + b, b = 100)
mtcars54 + 100

### tapply
x <- rnorm(6)
fac <- factor(c(1,1,1,2,2,2))
tapply(x, fac, mean)

my.data <- data.frame(x, fac)
rm(x)
rm(fac)
tapply(x, fac, mean) # returns an error if the two lines above are executed
with(my.data, tapply(x, fac, mean))
tapply(my.data$x, my.data$fac, mean)

plot(weight~feed, data=chickwts)

tapply(chickwts$weight, chickwts$feed, mean)
tapply(chickwts$weight, chickwts$feed, summary)

s <- tapply(chickwts$weight, chickwts$feed, function(x) c(mean(x), sd(x)))
s
typeof(s)

simplify2array(s)

### lapply
sapply(trims, function(tr) mean(y, tr))
lapply(trims, function(tr) mean(y, tr))

obj <- lapply(trims, function(tr) mean(y, tr))
unlist(obj)

is.data.frame(cars)
is.list(cars)
names(cars)
cars[1:2,]
lapply(cars, mean) # in this case the result is a list
sapply(cars, mean) # as lapply, but gives a vector if possible
apply(cars, 2, mean)

### sweep (case study)
mat <- matrix(rnorm(100), nrow=20) 
m1  <- scale(mat, center=TRUE, scale=TRUE)
m2  <- sweep(sweep(mat, 2, colMeans(mat), FUN="-"), 2, apply(mat, 2, sd), FUN="/")
all.equal(sum(m1 - m2), 0)

# to better understand the sweep solution, examine it line by line
centered <- sweep(mat, 2, colMeans(mat), FUN="-")
standardised <- sweep(centered, 2, apply(mat, 2, sd), FUN="/")

# even easier approach!
m3  <- apply(mat, 2, function(x) (x - mean(x))/sd(x))
all.equal(sum(m2 - m3), 0)

# however, the first approach is a lot faster if the columnwise means 
# and columnwise standard deviations are already known, e.g.

cmeans <- colMeans(mat) # same as apply(mat, 2, mean), notice we don't need to specify 2 for colMeans
csds <- apply(mat, 2, sd)

library(microbenchmark)
microbenchmark(sweep(sweep(mat, 2, cmeans, FUN="-"), 2, csds, FUN="/"), 
               apply(mat, 2, function(x) (x - mean(x))/sd(x)),
               scale(mat, TRUE, TRUE))

# notice that the means & sds are computed one by one for each column in the 2nd apply approach
# and we can't supply them as arguments in the 2nd apply approach either, unlike the recommended sweep approach