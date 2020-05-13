x <- seq(.1, .5, by=.1)
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

trims <- seq(.0, .5, by=.1)
trmeans <- numeric(length(trims))
for(i in 1:length(trims)) {
  trmeans[i] <- mean(y, trims[i])
}
trmeans
setNames(trmeans, trims)

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

colMeans(mtcars54) # apply(mtcars54, 2, mean)
rowMeans(mtcars54) # apply(mtcars54, 1, mean)
rowSums(mtcars54)  # apply(mtcars54, 1, sum)
colSums(mtcars54)  # apply(mtcars54, 2, sum)

### apply - example 2
my.function <- function(x, y) sum(x) * y
apply(mtcars54, 2, my.function) # returns an error
apply(mtcars54, 2, my.function, y = 2)

apply(mtcars54, 2, function(a, b) mean(a) + b, b = 100)
apply(mtcars54, 2, function(a, b) a + b, b = 100)
mtcars54 + 100

### tapply
x <- rnorm(6)
fac2 <- c(1,1,1,2,2,2)
fac <- factor(fac2)
tapply(x, fac, mean)
tapply(x, fac2, mean) # still works, fac2 is internally coerced to a factor
                      # still preferable to create the factor yourself though, for more control

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
