1:10

x <- c(5,3,8,1,.2)
y <- c(3,.7,-1,0,0)
x; y

n <- 1:10
n+1
n*1.25

x+y
x-y

x
x+1:2

x1 <- c(x, 2)
x1 + 1:2

n*1.25*.8 - n
n*.8*1.25 - n

set.seed(123)
x <- rnorm(100)
mean(x)
median(x)
sd(x)

t.test(x)
t.test(x, alternative = "greater")

mu.0 <- 0
xbar <- mean(x)
s <- sd(x)
n <- length(x)

t.stat <- (xbar - mu.0)/(s/sqrt(n))
t.stat

hist(x)

set.seed(1234)
y <- rnorm(100, mean = 3)

fivenum(x)
fivenum(y)
boxplot(x, y)

plot(sort(x), sort(y))

abline(3, 1)

abline(mean(y) - mean(x), 1,
       col = "red") 

cor.xy <- cor(sort(x), sort(y))
slope <- sd(y)/sd(x) * cor.xy
i <- mean(y) - slope*mean(x)

abline(i, s, col = "blue", lwd = 3)

lm(sort(y) ~ sort(x))
