# Ciarán Ó hAoláin - 17309103

# Q1
# (a)
ztest <- function (u0, stdev, sample)
  (mean(sample) - u0) / (stdev / sqrt(length(sample)))

q1a_test <-
  function ()
    ztest(0, 1, rnorm(20)) >= qnorm(0.05, lower.tail = FALSE)
q1a_test()
# FALSE
# H_0 is NOT rejected

# (b)
prop_fn_true <- function (n, f)
  sum(replicate(n, f())) / n
prop_fn_true(1000, q1a_test)
# 5.8% of the time, H_0 is rejected

# (c)
q1c_test <- function () {
  q1c_sample <- rnorm(20)
  q1c_sample[1] <- q1c_sample[1] + 10
  ztest(0, 1, q1c_sample) >= qnorm(0.05, lower.tail = FALSE)
}
q1c_test()
# We DO reject H_0

# (d)
prop_fn_true(1000, q1c_test)
# 72.3% of the time, H_0 is rejected
# The presence of the outlier makes us less likely to reject H_0

# (e)
npower <- function (delta = 0,
                    sigma = 1,
                    n = 20) {
  pnorm(qnorm(0.05, lower.tail = FALSE) - (delta * sqrt(n)) / sigma,
        lower.tail = FALSE)
}

# (f)
npower(sigma = 1,
       n = 20,
       delta = c(0, 0.2))
# 0.05, 0.226499
npower(sigma = 1,
       n = 100,
       delta = c(0, 0.2))
# 0.05, 0.63876

# (g)
curve(npower,-1, 2, xlab = expression(delta))
# This plot shows we're more likely reject H_0
# when the alternative the larger delta is.


# Q2
# When a=0, we divide by 0 yielding infinity.
# This is obviously incorrect, for example,
# if 3x-3=0, x=1. But qroots returns -Inf, NaN
# This is because -6/0 is -Inf, and 0/0 is NaN
# Obviosuly these answers are useless and wrong
# So let's rewrite the function to take these
# circumstances into consideration:
qroots <- function(a, b, c) {
  # Start changes
  if (a == 0) {
    if (b == 0) {
      roots <- c()
      return(roots)
    }
    root <- -c / b
    roots <- c(root, root)
    return(roots)
  }
  # End changes
  d <- b ^ 2 - 4 * a * c
  if (d > 0) {
    d1 <- sqrt(d)
    roots <-  c((-b - d1) / (2 * a), (-b + d1) / (2 * a))
    return(roots)
  }
  if (d == 0) {
    r <-  -b / (2 * a)
    roots <- c(r, r)
    return(roots)
  }
  roots <- c()
  return(roots)
}

qroots(1, -3, 2)
qroots(1, 4, 4)
qroots(1, 1, 1)

# Now these also work
qroots(0, 4, 0)
qroots(0, 4, 8)


# Q3
# (a)
fib <- function (n) {
  if (n <= 2)
    tail(c(1, 1), n = n)
  else {
    prev <- fib(n - 1)
    c(prev, sum(tail(prev, n = 2)))
  }
}
fib(80)
# 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...

# (b)
fib_30 <- fib(30)
fib_30_seq <- fib_30[2:30] / fib_30[1:29]
plot(1:29, fib_30_seq)
# The sequence appears to converge to 1.618034

# (c)
(1 + sqrt(5)) / 2
# 1.618034
# The sequence IS converging to this ratio

# Q4
# (a)
num_arrivals <- function(time_windows)
  rpois(time_windows, 2 / 5)
sum(num_arrivals(180))
mean(replicate(100000, sum(num_arrivals(180))))
# 72

# (b)
sims <- replicate(3, num_arrivals(180))
sums <- apply(sims, 2, cumsum)
plot(
  sums[, 1],
  col = 2,
  type = "l",
  ylim = c(0, max(sums)),
  xlab = "",
  ylab = ""
)
lines(sums[, 2], col = 3)
lines(sums[, 3], col = 4)

# (c)
title("Total Customers Arrived during Simulation",
      xlab = "Minutes",
      ylab = "Arrivals")
legend(
  0,
  y = max(sums),
  legend = c("sim 1", "sim 2", "sim 3"),
  fill = 2:4
)

# Q5
# (a)
current_value <- 3 # n1 initial value
R <- 1.4
K <- 10
time <- 30
list_of_values <- c(current_value)
for (i in 2:time) {
  current_value  <-
    (K * R * current_value) / (K + current_value * (R - 1))
  list_of_values <- c(list_of_values, current_value)
}
current_value
# 9.99865
end_answer_q5a <- current_value

list_of_values
# 3.000000 3.750000 4.565217  ... 9.998650
answer_q5a <- list_of_values


# (b)
bhmodel <- function (R, K, n1, time) {
  if (time == 1) {
    return(c(n1))
  }
  prev <- bhmodel(R, K, n1, time - 1)
  n_minus1 <- tail(prev, n = 1)
  return(c(prev, (K * R * n_minus1) / (K + n_minus1 * (R - 1))))
}
bhmodel(1.4, 10, 3, 30)
# 3.000000 3.750000 4.565217  ... 9.998650
# Same final value as above

# My function returns a vector with value at every step,
# if the function only needs the final value then we can use this:
bhmodel_final_value <-
  function (R, K, n1, time)
    tail(bhmodel(R, K, n1, time), n = 1)
bhmodel_final_value(1.4, 10, 3, 30)
# 9.99865 as above, again

# (c)
r_vals <- seq(1.4, 2, 0.2)
evaluated_scenarios <-
  sapply(r_vals, function (R)
    bhmodel(
      R = R,
      K = 10,
      n1 = 3,
      time = 30
    ))
plot(
  evaluated_scenarios[, 1],
  type = "l",
  col = 2,
  xlab = "Time Step",
  ylab = "Population"
)
for (i in 2:length(r_vals))
  lines(evaluated_scenarios[, i], col = i + 1)
title("Population over Time")
legend(
  25,
  y = 8,
  title = "Value of R",
  legend = r_vals,
  fill = 2:(length(r_vals) + 1)
)

# (d)
explicit_nt <- function(R, K, n1, time)
  c(n1, (K * n1) / (n1 + (K - n1) * R ^ (-(1:29))))

# Check all the values
all.equal(explicit_nt(1.4, 10, 3, 1:30), answer_q5a)
# TRUE
# They are the same

# (e)
paramecium <-
  c(2, 3, 29, 92, 173, 210, 240, 240, 240, 240, 219, 255, 252, 270, 240, 249)

y <- paramecium[-length(paramecium)] / paramecium[-1]
x <- paramecium[-length(paramecium)]

fit <- lm(y ~ x)
betas <- coef(fit)
beta0 <- betas[1]
beta1 <- betas[2]

R.hat <- 1 / beta0
K.hat <- (R.hat - 1) / (R.hat * beta1)

plot(paramecium)
fitted <- bhmodel(
  R = R.hat,
  K = K.hat,
  n1 = paramecium[1],
  time = length(paramecium)
)
lines(fitted, col = 4, lwd = 2)  

