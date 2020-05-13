### Keefe Murphy
### 12345678

## Q1
getwd()
#setwd("D:/Google Drive/Maynooth/ST366 Statistical Computing with R/2019-2020/Exams/")
grades <- read.csv("D:/Google Drive/Maynooth/ST366 Statistical Computing with R/2019-2020/Exams/student_grades.csv", header = TRUE)
str(grades)

head(grades)
sum(is.na(grades[,1:4]))

# a)
grades[,1:4][is.na(grades[,1:4])] <- 0
# alternatively
# grades[,1:4] <- replace(grades[,1:4], is.na(grades[,1:4]), 0)

sum(is.na(grades[,1:4])) # check!

# b)
grades$homework_average <- apply(grades[,1:4], 1, mean)
# alternatively
#grades$homework_average <- rowMeans(grades[,1:4])

# c)
grades$midterm[is.na(grades$midterm)] <- grades$homework_average[is.na(grades$midterm)]

# d)
# i.
y <- grades$midterm
X <- model.matrix(~ homework_average, data=grades)

# ii.
beta.hat <- solve(crossprod(X, X)) %*% crossprod(X, y)

# same as
# beta.hat <- solve(t(X) %*% X) %*% (t(X) %*% y)

# alternatively
 mod <- lm(midterm ~ homework_average, data=grades)
 beta.hat2 <- coef(mod)

# iii.
y.hat <- X %*% beta.hat

# iv.
plot(midterm ~ homework_average, data=grades)
lines(y.hat ~ homework_average, data=grades)
# lines(y.hat ~ grades$homework_average)
# abline(beta.hat[1], beta.hat[2], col="red")

# e)
hist(grades$midterm, prob=TRUE)
# hist(grades$midterm, freq=FALSE)
lines(density(grades$midterm))                   # accept the default: an optimally tuned bandwidth
#lines(density(grades$midterm, bw=3), col="red") # manually specify a bandwidth

# f)
mu.hat <- mean(grades$midterm)
sigma.hat <- sd(grades$midterm)
curve(dnorm(x, mu.hat, sigma.hat), 
      add=TRUE, col="blue", lwd=2)

## Q2
# a)
gameA <- sample(1:6, 1000, replace=TRUE)
gameB <- apply(data.frame(sample(1:6, 1000, replace=TRUE),
                          sample(1:6, 1000, replace=TRUE)), 1, max)

# alternatively
gameB2 <- pmax(sample(1:6, 1000, replace=TRUE), sample(1:6, 1000, replace=TRUE))
# this WILL NOT give the same result because of the different random samples drawn
# if we use the same random samples, we can see that both approaches to computing the max give the same result

# i.e.
d1 <- sample(1:6, 1000, replace=TRUE)
d2 <- sample(1:6, 1000, replace=TRUE)

gameB3 <- apply(data.frame(d1, d2), 1, max)
gameB4 <- pmax(d1, d2)
identical(gameB3, gameB4) # TRUE

# b)
# we can do this without a loop by exploiting vectorisation
# remember, "c(1,2,3) - 3" is the same as "c(1,2,3) - c(3,3,3)"
profitA <- gameA - 3
profitB <- gameB - 3.5
mean(profitA) # your answers will be different due to random number generation
mean(profitB)

# game B is more profitable
# to be explicit about it
names(which.max(c(A=mean(profitA), B=mean(profitB)))) # answer is "B"

# a brief explanation:
# the expected value of any single die roll is 3.5
# the expected value of the max of two die can be easily computed as 161/36 (approx. 4.47)
# so, both games are profitable on average provided their respective costs are less than these values
# in other words, "mean(gameA - 3.5)" and "mean(gameB - 161/36)" should both be near-zero or negative
#