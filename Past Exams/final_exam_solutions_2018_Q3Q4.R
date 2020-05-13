### Keefe Murphy
### 12345678

## Q3
?distributions
?rbinom

# a)
sum(rbinom(n=100, size=7, prob=0.5) >= 4)/100

1 - pbinom(4, 7, 0.5)

# b)
mean(replicate(10000, sum(rbinom(n=100, size=7, prob=0.5) >= 4)/100))

# The question asked to estimate the probability of winning "at least four games",
# i.e. a majority of the 7 games, hence the ">=" sign.

# But the so-called "exact answer" in terms of pbinom is nowhere near our simulated approximation!
# Actually, the exact answer would really correspond to using a ">" sign, without equality.

# In the ">=" scenario, the exact answer is actually
1 - pbinom(3, 7, 0.5) # 0.5

# now, this makes sense; the probability Dublin win >= 4 games is 
# 1 minus the sum of the probability they win EXACTLY 3, EXACTLY 2, EXACTLY 1, and EXACTLY 0 games,
# which is given by pbinom(3, 7, 0.5) because pbinom computes CUMULATIVE probabilities.

# alternatively,
pbinom(3, 7, 0.5, lower.tail=FALSE) # 0.5

# compare this to:
system.time(answer <- mean(replicate(100000, sum(rbinom(n=1000, size=7, prob=0.5) >= 4)/1000)))
answer

# now that we know pbinom can be used to estimate cumulative probabilies,
# we can examine all possible outcomes on the CDF
barplot(pbinom(0:7, 7, 0.5), names.arg=0:7) # probabilities of x taking a value <= X

# and also on the PMF
barplot(dbinom(0:7, 7, 0.5), names.arg=0:7) # probabilities of winning exactly X games

# the notion of p****() being a CDF and d****() being a PMF/PDF is not really clear from
# the distribution help files... but now you know!

# actually,
all.equal(cumsum(dbinom(0:3, 7, .5)), 
          pbinom(0:3, 7, 0.5))

## Q4
require(MASS)

# a)
hist(Boston$medv)
hist(Boston$medv, breaks = 23)
## the distribution seems to be right-skewed, with a large number of
## occurrences of the value 50

# b)
out_index <- function(x) {
  xbar    <- mean(x)
  s       <- sd(x)
  ll      <- xbar - 2 * s
  ul      <- xbar + 2 * s
  which(x  < ll   | x > ul)
}

out_index(Boston$medv)

# c)
oi <- out_index(Boston$medv)
plot(crim[-oi] ~ medv[-oi], data = Boston, xlim = c(0,50))
points(crim[oi] ~ medv[oi], data = Boston, pch = 4)

# d)
out_Boston <- apply(Boston, 2, out_index)

# e)
n_out <- sapply(out_Boston, length)
# alternatively
# n_out <- lengths(out_Boston)
barplot(sort(n_out, decreasing = TRUE))

# side note: xbar +/- 2 * s is clearly shorthand for xbar +/- 1.96 * s
# although, 1.96 isn't exactly precise either
# look at "qnorm(0.975)" for the exact critical value

# as an exercise (!), redo this question following the steps below:
# i. create a copy of "out_index" called "out_index2".
# ii. add an extra argument "crit" to the out_index2 function.
# iii. replace "2" in the body of the function with "crit".
# iv. Let crit=qnorm(0.975) by default.
# v. Repeat the rest of the question using out_index2 with this default value.

out_index2 <- function(x, crit=qnorm(0.975)) {
  # Note: "qnorm(0.975)" is equal to "qnorm(0.025, lower.tail=FALSE)" is equal to "-qnorm(0.025)"
  xbar    <- mean(x)
  s       <- sd(x)
  ll      <- xbar - crit * s
  ul      <- xbar + crit * s
  which(x  < ll   | x > ul)
}
out_index2(Boston$medv)

# this code chunk takes an alternative approach to part c)
plot(crim ~ medv, data=Boston,
     pch=replace(rep(1, nrow(Boston)), out_index2(Boston$medv), 4))

out_Boston2 <- apply(Boston, 2, out_index2)

n_out2 <- lengths(out_Boston2)
barplot(sort(n_out2, decreasing = TRUE))
#