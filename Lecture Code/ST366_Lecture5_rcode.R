alist <- list(4,"abc", c(2,3,7))
alist
alist[[1]] # the first component
alist[1] # sublist consisting of first component
alist[[3]] # the third element
alist[[3]][2] # second element of the third list component

length(alist) # number of list components
lengths(alist) # the length of each list element

names(alist) <- c("first","second", "third")
alist$second
alist[[2]]
alist[["second"]]
mary <- list(firstname="Mary", surname="Sullivan", age=21)
typeof(mary)
mary$age
names(mary)

head(mtcars)
is.list(mtcars)
is.data.frame(mtcars)
mtcars$wt

data(mtcars)
f <- lm(mpg ~ wt, data = mtcars)
is.list(f)
str(f)
summary(f)

set.seed(2018); y1 <- rnorm(10, 2, 5)
t1 <- t.test(y1)
str(t1)

set.seed(1234); y2 <- rnorm(15, 3, 5)
t.test(y1, y2, var.equal = TRUE)

f <- t.test(y1, y2, var.equal = TRUE)
typeof(f)
names(f)
f$statistic

y <- c(y1, y2)
g <- c(rep(1,10), rep(2,15))
d <- data.frame(y = y, g = factor(g))
d[1:3,]
t.test(y ~ g, data = d, var.equal = TRUE)

head(sleep)

plot(extra ~ group, data=sleep)

t.test(extra ~ group, data = sleep, var.equal = TRUE)

t.test(extra ~ group, data = sleep)

fit <- lm(extra ~ group, data = sleep)
summary(fit)
anova(fit) 
# ST364 students: aov() is the function used to fit ANOVA models, 
# anova() just extracts the ANOVA table from an appropriate fitted object

names(fit)
fit$residuals
residuals(fit)

par(mfrow = c(2,2))
plot(fit)

class(fit)
?plot.lm
