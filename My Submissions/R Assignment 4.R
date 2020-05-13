# Ciarán Ó hAoláin - 17309103

# Q1
## (a)
loan <- function (amount, interest, repayment) {
  if (amount < 0) {
    return(c(amount))
  } else {
    return(c(amount, loan(
      amount * (1 + interest / 12) - repayment, interest, repayment
    )))
  }
}

loan_balances <- loan(1000, .11, 12)
length(loan(1000, .11, 12)) - 1
# 159 months

## (b)
plot(loan_balances, xlab = "Month", ylab = "Amount Owed")

## (c)
total_paid <- (158 * 12) + 2.246233 * (1 + .11 / 12)
total_paid - 1000
# 898.27 paid in interest


# Q2
## (a)
eupop <-
  read.table("~/st366/eupop.txt", header = TRUE, row.names = 1)
sapply(eupop, mean)
# p014        p1544       p4564       p65.      pop
# 17.70000    43.10000    23.78000    15.45333  25030.52000

# This mean doesn't consider the size of each country's population.
# Ireland shouldn't have as much influence on the mean as Germany,due to
# the difference in proportion of the total EU population they represent.

## (b)
weighted_populations <-
  sapply(eupop[,-5], weighted.mean, w = eupop$pop)
weighted_populations
# p014      p1544     p4564     p65.
# 17.00021  42.98610  23.99481  16.05757

## (c)
sum(weighted_populations)
# 100.0387

## (d)
lowest_row_nums <- apply(eupop[-5], c(2), which.min)
rownames(eupop[-5])[lowest_row_nums]
# p014      p1544       p4564       p65.
# "Italy"   "Sweden"    "Ireland"   "Ireland"


# Q3
## (a)
x <- rnorm(100, mean = 3, sd = 5)
g <- rep(1:20, each = 5)
y <- tapply(x, g, mean)

## (b)
alt_y <- numeric(20)
for (i in 1:20) {
  alt_y[i] <- mean(x[(1 + (i - 1) * 5):(5 + (i - 1) * 5)])
}
all.equal(as.numeric(y), alt_y)
# TRUE

## (c)
index <- seq(2.5, 97.5, length = 20)
plot(index,
     y,
     ylim = range(x) * 1.1,
     type = "o",
     col = "red")
points(x, cex = .8, pch = 21, bg = "lightgray")

## (d)
ul <- 3 + 1.96 * 5 # 12.8
ll <- 3 - 1.96 * 5 # -6.8
abline(h = ul, lty = 2)
abline(h = ll, lty = 2)
abline(h = 3, lty = 2)

## (e)
is_outlier <- function(x)
  x > ul | x < ll
outliers <- Filter(is_outlier, x)

## (f)
outlier_indices <- which(sapply(X = x, FUN = is_outlier))
points(outlier_indices, x[outlier_indices], pch = 21, bg = 4)

## (g)
test0 <- function(x, ll, ul)
  which(sapply(
    X = x,
    FUN = function(x)
      x > ul | x < ll
  ))

## (h)
test0(x, ll, ul)

# Q4
jump <- function(x) {
  max(abs(diff(x)))
}
jump(x)

# Q5
A <- matrix(rep(0:4, each = 5) + seq(5), nrow = 5)

henkel <- function(n)
  matrix(rep(0:(n - 1), each = n) + seq(n), nrow = n)

henkel(10)
henkel(12)

# Q6
## (a)
X <- matrix(sapply(
  X = 1:4,
  FUN = function(x)
    c(x, x ^ 2)
), ncol = 2)
X

## (b)
A <- t(X) %*% X
A
# 22  84
# 84  362
A_inv <- solve(A)
A_inv
# 0.39867841    -0.09251101
# -0.09251101   0.02422907
all.equal(crossprod(A, A_inv), diag(2))
# TRUE, meaning the cross product is the identity
# Showing that the matrix multiplied by its inverse is the identity

## (c)
B <- X %*% t(X)
# 10   28   14   52
# 28   82   38  148
# 14   38   20   72
# 52  148   72  272
B_inv <- solve(B)
# error, as expected, since:
all.equal(det(B), 0)
# The determinant being 0 means the matrix is not invertable.

