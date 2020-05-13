# Ciarán Ó hAolain 17309103

# Q1
x <- 1:10
x * (2/3) * (3/2) - x # [1]  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00 -8.881784e-16  0.000000e+00  0.000000e+00  0.000000e+00
# Here, we start with x, a list of the numbers 1 to 10. 
# First we multiply by (2*3)*(3/2) which is equal to 1, and should therefore
# leave us with just x.
# We then subtract x, which should leave us with 0.
# The actual answer differs slightly from 0 due to how the numbers are
# represented in binary as a floating point number.


# Q2
2 + 2 == 4 # [1] TRUE
sqrt(2) * sqrt(2) == 2 # [1] FALSE
all.equal(sqrt(2) * sqrt(2), 2) # [1] TRUE
# The first comparison is obviously true.
# The second comparison fails, due to how sqrt(2) is represented in binary as a
# floating point number.
# This leads to sqrt(2)*sqrt(2) being slightly different from exactly 2.
# The third comparsion uses all.equal. Unlike the normal == operator,
# all.equal allows a slight difference between the things it is checking.
# Therefore, we find that the results are (nearly) equal.


# Q3
rep(0:4, each=5) + seq(5) # [1] 1 2 3 4 5 2 3 4 5 6 3 4 5 6 7 4 5 6 7 8 5 6 7 8 9


# Q4
n <- c(100, 200, 400, 800)
for (a in n) {
  print(sum(1:a) == (a*(a + 1))/2)
}
# [1] TRUE
# [1] TRUE
# [1] TRUE
# [1] TRUE
# These sequences are identical, component-wise.


# Q5
x <- 1/3
n <- 10
sum(x^(0:n)) # [1] 1.499992
sum(x^(0:n)) == (1-x^(n+1))/(1-x) # [1] TRUE
# These sequences are identical.


# Q6
# a.
solar.radiation <- c(11.1, 10.6, 6.3, 8.8, 10.7, 11.2, 8.9, 12.2)

# b.
mean(solar.radiation) # [1] 9.975
median(solar.radiation) # [1] 10.65
var(solar.radiation) # [1] 3.525

# c.
sr10 <- solar.radiation + 10 # [1] 
mean(sr10) # [1] 19.975
median(sr10) # [1] 20.65
var(sr10) # [1] 3.525
# The variance does not change, however the mean and median both increase by 10 respectively.

# d.
sr2 <- solar.radiation * -2 # [1] -22.2 -21.2 -12.6 -17.6 -21.4 -22.4 -17.8 -24.4
mean(sr2) # [1] -19.95
median(sr2) # [1] -21.3
var(sr2) # [1] 14.1
mean(sr2) / mean(solar.radiation) # [1] -2
median(sr2) / median(solar.radiation) # [1] -2
var(sr2) / var(solar.radiation) # [1] 4
# The mean and median are both multiplied by -2, the variance is multiplied by 4.

hist(solar.radiation)
hist(sr10)
hist(sr2)
