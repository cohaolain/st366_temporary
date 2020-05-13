# Ciarán Ó hAoláin - 17309103

# Q1
## (a)
matrix(t(sapply(c(2, 1, 3, 7, 6), function(a)
  a ** seq(0, 3))), ncol = 4)

## (b)
powermat <- function (x, m) {
  matrix(t(sapply(x, function(a)
    a ** seq(0, m - 1))), ncol = m)
}
powermat(c(2, 1, 3, 7, 6), 4)

## (c)
powermat(c(1, 2, 3, 4, 0), 5)

# Q2
y <- c(rnorm(10, mean = 20, sd = 3), rnorm(10, mean = 10, sd = 3))
x <- gl(2, 10)
X <- model.matrix( ~ x)

## (a)
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta.hat
coef(lm(y ~ x))
# These are identical

## (b)
rms <-
  (t(y) %*% y - t(y) %*% X %*% beta.hat
   - (t(beta.hat) %*% t(X) %*% y)
   + (t(beta.hat) %*% t(X) %*% X %*% beta.hat)) / 18
rms
anova(lm(y ~ x))$"Mean Sq"[2]
# These are identical

## (c)
H <- solve(t(X) %*% X) * as.numeric(rms)
standard_error <- sqrt(diag(H))
standard_error
summary(lm(y ~ x))$coef[, 2]
# These are identical

# Q3
## (a)
correct_answers <- c(rbinom(1, 20, .25), rbinom(1, 10, .5))
# correct_answers
sum(correct_answers)
# 5 + 5 = 10 correct answers

## (b)
prob_passed <- function(correct_answers) {
  length(Filter(function(x)
    x >= 12, correct_answers)) / length(correct_answers)
}


n <- 1000
correct_answers <-
  replicate(n, c(rbinom(1, 20, .25) + rbinom(1, 10, .5)))
prob_passed(correct_answers)
hist(correct_answers)
# 27.114%

## (c)
correct_answers <-
  replicate(n, c(rbinom(1, 20, .33) + rbinom(1, 10, .5)))
prob_passed(correct_answers)
hist(correct_answers)
# 50%

## (d)
grade <- function(is_corrects,
                  correct_grade,
                  incorrect_grade) {
  sapply(is_corrects, function (is_correct)
    ifelse(is_correct == 1, correct_grade, incorrect_grade))
}
sim_scores <- function() {
  first_section <- replicate(20, sample(c(0, 0, 0, 1), 1))
  second_section <- replicate(10, sample(c(0, 1), 1))
  # Grade it
  grades <-
    c(grade(first_section, 1,-1 / 3), grade(second_section, 1,-1))
  return (grades)
}
sum(sim_scores())

big_simulation <- replicate(1000, sum(sim_scores()))
prob_passed(big_simulation)
hist(big_simulation)
# 0.2294%

# Q4
## (a)
mcInt <- function(g, a, b, n = 10000) {
  u <- runif(n, a, b)
  return(mean(g(u)) * (b - a))
  
}

answer <- mcInt(function (x)
  x ^ 2, 1, 3)
# 8.65834
actual <- 8 + 2 / 3
error <- abs(answer - actual)
# 0.0492

## (b)
answer <- mcInt(function (x)
  exp(x), 1, pi)
# 20.52548
actual <- 20.422
error <- abs(answer - actual)
# 0.0024

## (c)
mcIntExp <- function (g, n = 10000) {
  x <- rexp(n)
  mean(g(x) / dexp(x))
}
g <- function(x)
  exp(-1 * x ** 3)
answer <- mcIntExp(g)
# 0.89664
actual <- 0.89298
error <- abs(answer - actual)
# 0.0037

## (d)
answer <- mcInt(function (x)
  (1 / sqrt(2 * pi)) * exp(-(x ** 2) / 2), 0, 1)
# 0.34152
actual <- 0.341345
error <- abs(answer - actual)
# 0.0002

# Q5
## (a)
set.seed(17309103)

## (b)
box_muller <- function (npairs) {
  gen_pair <- function () {
    u <- runif(2)
    theta <- 2 * pi * u[1]
    R <- sqrt(2 * (-log(u[2])))
    return(c(R * cos(theta), R * sin(theta)))
  }
  return(replicate(npairs, gen_pair()))
}
pairs <- box_muller(5000)
length(pairs)

## (c)
mu <- mean(pairs)
sigma <- sd(pairs)
hist(pairs, freq = FALSE)
curve(dnorm(x, mu, sigma), add = TRUE)
curve(dnorm(x), add = TRUE, col = "red")

## (d)
n_10_3 <- box_muller(5000) * 3 + 10
mu <- mean(n_10_3)
sigma <- sd(n_10_3)
hist(n_10_3, freq = FALSE, ylim = c(0, .15))
curve(dnorm(x, mu, sigma), add = TRUE)
curve(dnorm(x, 10, 3), add = TRUE, col = "red")

## (e)
### i.
biv <- matrix(replicate(10000, box_muller(1)),
              nrow = 2,
              ncol = 10000)
nrow(biv)
ncol(biv)

### ii.
SIGMA <- matrix(c(2, -1, -1, 2), nrow = 2)
SIGMA

### iii.
SIG12 <- chol(SIGMA)

### iv.
MU <- c(1, 7)

### v.
BIV <- MU + SIG12 %*% biv
nrow(BIV)
ncol(BIV)

### vi.
require(MASS)
contour(kde2d(BIV[1, ], BIV[2, ]))
